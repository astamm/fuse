#' \code{R6} Class for Functional Data
#'
#' @docType class
#'
#' @usage FunctionalData$new(grid = c(-5, 5), value = dnorm, resolution = 100L)
#'
#' @export
#' @keywords data
#' @return A \code{\link{FunctionalData}} object with a set of specific methods
#'   for functional data.
#' @format An \code{\link[R6]{R6Class}} generator object for generating
#'   \code{\link{FunctionalData}} objects.
#'
#' @examples
#' f <- SobolevData$new(value = dnorm, grid = c(-5, 5))
#' g <- BayesData$new(value = dnorm, grid = c(-5, 5))
#' f
#' g
#' f$plot()
#' g$plot()
#' f$add(f)
#' g$add(g)
#' f$add(f)$plot()
#' f$add()$plot()
#' f$mul(2)$plot()
#' g$add(g)$plot()
#' g$mul(2)$plot()
#' g$mul(1/2)$plot()
#' g$div(2)$plot()
#' f$mul(f)$plot()
#' g$mul(g)$plot()
#' f$prod(f$mul(2))
#' f$dist(f$mul(2))
#' f$dist(f$mul(2), normalize = TRUE)
#'
#' @field range Stores the grid range over which the functional datum is
#'   defined.
#'
#' @section Methods: \describe{ \item{\code{new(grid = c(0, 2 * pi), value =
#'   sin, resolution = 1000L)}}{This method is used to create object of this
#'   class with \code{serveraddress} as address of the server object is
#'   connecting to.} \item{\code{format(...)}}{This method changes server that
#'   you are contacting with to \code{serveraddress}.}
#'   \item{\code{evaluate(x)}}{This method creates new session on the server
#'   with optionally given name in \code{sessionname}.}
#'   \item{\code{plot()}}{This method changes currently used session on the
#'   server to the one with id given in \code{sessionid} parameter.} }
FunctionalData <- R6::R6Class(
  classname = "FunctionalData",
  cloneable = FALSE,
  public = list(
    initialize = function(grid = c(-5, 5), value = dnorm, resolution = 100L) {
      # If value is a function, then grid can be either a range or a grid and the grid
      # is then resampled to the input resolution.
      # If value is a vector, then grid must be a vector of same size as
      # value and this size defines the resolution, regardless of the input resolution.

      stopifnot(is.numeric(grid) || "Date" %in% class(grid))

      if (is.numeric(value)) {
        resolution <- length(value)
        stopifnot(length(grid) == resolution)
        private$.resolution <- resolution
        private$.range <- range(grid)
        private$.value <- stats::splinefun(grid, value)
      } else {
        stopifnot(is.integer(resolution), length(resolution) == 1L)
        stopifnot(length(grid) >= 2L)
        private$.value <- rlang::as_function(value)
        if (length(grid) == 2L) { # Consider it is giving the grid range
          stopifnot(grid[1] < grid[2])
          private$.resolution <- resolution
          private$.range <- grid
        } else {
          private$.resolution <- min(resolution, length(grid))
          private$.range <- range(grid)
        }
      }
    },
    format = function(...) {
      c(
        paste0("Functional Data"),
        paste0("  Grid size -------> ", private$.resolution, " points"),
        paste0("  Validity range --> [", toString(round(private$.range, digits = 2L)), "]"),
        paste0("  Available methods: "),
        paste0("    ", names(self))
      )
    },
    evaluate = function(x) {
      private$.value(x)
    },
    copy = function() {
      FunctionalData$new(
        grid = private$.range,
        value = private$.value,
        resolution = private$.resolution
      )
    },
    plot = function() {
      df <- tibble::tibble(
        t = seq(private$.range[1], private$.range[2], len = private$.resolution),
        `f(t)` = private$.value(t)
      )
      ggplot2::ggplot(df, ggplot2::aes(t, `f(t)`)) +
        ggplot2::geom_line() +
        ggplot2::theme_bw()
    },
    add = function(x) {
      invisible(self)
    },
    sub = function(x) {
      invisible(self)
    },
    mul = function(x) {
      invisible(self)
    },
    div = function(x) {
      invisible(self)
    },
    pow = function(x) {
      invisible(self)
    },
    apply = function(fun, ...) {
      grid <- seq(private$.range[1], private$.range[2], len = private$.resolution)
      values <- sapply(grid, private$.value)
      self$initialize(grid, rlang::as_function(fun)(values, ...), private$.resolution)
      invisible(self)
    },
    integrate = function(lower, upper, measure = "Lebesgue") {
      lb <- private$.range[1]
      if (missing(lower))
        lower <- lb
      else
        stopifnot(lower >= lb)
      ub <- private$.range[2]
      if (missing(upper))
        upper <- ub
      else
        stopifnot(upper <= ub)
      switch (measure,
        Lebesgue = integrate(self$evaluate, lower, upper)$value,
        NA_real_
      )
    },
    dist = function(x, normalize = FALSE) {
      NA_real_
    },
    normdist = function(x) {
      NA_real_
    }
  ),
  active = list(
    range = function(value) {
      if (missing(value))
        private$.range
      else
        stop("`$range` is read-only.", call. = FALSE)
    },
    resolution = function(value) {
      if (missing(value))
        private$.resolution
      else
        stop("`$resolution` is read-only.", call. = FALSE)
    }
  ),
  private = list(
    .resolution = integer(0L),
    .range = numeric(0L),
    .value = NULL,
    .update = function(x) {
      stopifnot("FunctionalData" %in% class(x) || is.numeric(x))
      if ("FunctionalData" %in% class(x)) {
        resolution <- min(private$.resolution, x$resolution)
        lb <- max(private$.range[1], x$range[1])
        ub <- min(private$.range[2], x$range[2])
        range <- c(lb, ub)
        grid <- seq(lb, ub, by = (ub - lb) / (resolution - 1))
        x <- x$evaluate(grid)
        y <- private$.value(grid)
      } else {
        lx <- length(x)
        stopifnot(lx >= 1L)
        if (lx == 1L) { # Scalar operation does not modify grid
          resolution <- private$.resolution
          x <- rep(x, resolution)
        } else # Vector addition does modify grid
          resolution <- min(private$.resolution, lx)
        grid <- seq(private$.range[1], private$.range[2], len = resolution)
        y <- private$.value(grid)
      }
      list(grid = grid, x = x, y = y, resolution = resolution)
    }
  )
)
