#' @usage BayesData$new(grid = c(-5, 5), value = dnorm, resolution = 100L)
#' @include FunctionalData.R
#' @export
#' @rdname FunctionalData
BayesData <- R6::R6Class(
  classname = "BayesData",
  cloneable = FALSE,
  inherit = FunctionalData,
  public = list(
    format = function(...) {
      c(
        super$format(...),
        paste0("  Space ----------> Bayes")
      )
    },
    copy = function() {
      BayesData$new(
        grid = private$.range,
        value = private$.value,
        resolution = private$.resolution
      )
    },
    add = function(x) {
      if ("FunctionalData" %in% class(x))
        stopifnot("BayesData" %in% class(x))
      x <- private$.update(x)
      self$initialize(grid = x$grid, value = x$y * x$x, resolution = x$resolution)
      invisible(self)
    },
    sub = function(x) {
      if ("FunctionalData" %in% class(x))
        stopifnot(" BayesData" %in% class(x))
      x <- private$.update(x)
      self$initialize(grid = x$grid, value = x$y / x$x, resolution = x$resolution)
      invisible(self)
    },
    mul = function(x) {
      if ("FunctionalData" %in% class(x))
        stopifnot(" BayesData" %in% class(x))
      x <- private$.update(x)
      self$initialize(grid = x$grid, value = x$y ^ x$x, resolution = x$resolution)
      invisible(self)
    },
    div = function(x) {
      if ("FunctionalData" %in% class(x))
        stopifnot(" BayesData" %in% class(x))
      x <- private$.update(x)
      self$initialize(grid = x$grid, value = x$y ^ (1 / x$x), resolution = x$resolution)
      invisible(self)
    }
  )
)
