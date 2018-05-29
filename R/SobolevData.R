#' @usage SobolevData$new(grid = c(-5, 5), value = dnorm, resolution = 100L)
#' @include FunctionalData.R
#' @export
#' @rdname FunctionalData
SobolevData <- R6::R6Class(
  classname = "SobolevData",
  cloneable = FALSE,
  inherit = FunctionalData,
  public = list(
    format = function(...) {
      c(
        super$format(...),
        paste0("  Space ----------> Sobolev")
      )
    },
    copy = function() {
      SobolevData$new(
        grid = private$.range,
        value = private$.value,
        resolution = private$.resolution
      )
    },
    add = function(x) {
      if ("FunctionalData" %in% class(x))
        stopifnot("SobolevData" %in% class(x))
      x <- private$.update(x)
      self$initialize(grid = x$grid, value = x$y + x$x, resolution = x$resolution)
      invisible(self)
    },
    sub = function(x) {
      if ("FunctionalData" %in% class(x))
        stopifnot("SobolevData" %in% class(x))
      x <- private$.update(x)
      self$initialize(grid = x$grid, value = x$y - x$x, resolution = x$resolution)
      invisible(self)
    },
    mul = function(x) {
      if ("FunctionalData" %in% class(x))
        stopifnot("SobolevData" %in% class(x))
      x <- private$.update(x)
      self$initialize(grid = x$grid, value = x$y * x$x, resolution = x$resolution)
      invisible(self)
    },
    div = function(x) {
      if ("FunctionalData" %in% class(x))
        stopifnot("SobolevData" %in% class(x))
      x <- private$.update(x)
      self$initialize(grid = x$grid, value = x$y / x$x, resolution = x$resolution)
      invisible(self)
    },
    pow = function(x) {
      if ("FunctionalData" %in% class(x))
        stopifnot("SobolevData" %in% class(x))
      x <- private$.update(x)
      self$initialize(grid = x$grid, value = x$y ^ x$x, resolution = x$resolution)
      invisible(self)
    },
    inner = function(x) {
      if ("FunctionalData" %in% class(x))
        stopifnot("SobolevData" %in% class(x))
      self$mul(x)$integrate()
    },
    norm = function() {
      sqrt(self$inner(self))
    },
    normalize = function() {
      self$div(self$norm())
    },
    dist = function(x, normalize = FALSE) {
      if ("FunctionalData" %in% class(x))
        stopifnot("SobolevData" %in% class(x))
      out <-
        if (!normalize) return(self$sub(x)$pow(2L)$integrate())
      p <- self$sub(x)$pow(2L)
      p$integrate() / as.numeric(diff(p$range))
    },
    normdist = function(x) {
      if ("FunctionalData" %in% class(x))
        stopifnot("SobolevData" %in% class(x))
      x <- private$.update(x)
      e1 <- SobolevData$new(grid = x$grid, value = x$y, resolution = x$resolution)
      e2 <- SobolevData$new(grid = x$grid, value = x$x, resolution = x$resolution)
      e1$norm() - e2$norm()
    }
  )
)
