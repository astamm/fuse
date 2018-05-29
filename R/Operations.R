#' \code{FunctionalData} Group Generic Functions
#'
#' These functions provide methods for applying
#' \code{\link[base]{groupGeneric}}s to \code{\link{FunctionalData}} objects.
#' Note that unary operators in \code{Ops} group (\code{"&"}, \code{"|"} and
#' \code{"!"}) are not meaningful for \code{\link{FunctionalData}} objects and
#' thus not implemented.
#'
#' @inherit base::groupGeneric
#'
#' @examples
#' f <- SobolevData$new(c(0, 2 * pi), ~ .)
#' g <- sin(f)
#' z1 <- f + g
#' y0 <- 1:10
#' z2 <- f + y0
#' @name groupGeneric.FunctionalData
#' @include FunctionalData.R
NULL

#' @export
#' @rdname groupGeneric.FunctionalData
Math.FunctionalData <- function(x, ...) {
  x$copy()$apply(.Generic, ...)
}

#' @export
#' @rdname groupGeneric.FunctionalData
Ops.FunctionalData <- function(e1, e2) {
  if (missing(e2)) {
    # code for unary operations
    stop("Unary operators &, | and ! are not meaningful for FunctionalData objects.")
  } else {
    # code for binary operations
    fd1 <- "FunctionalData" %in% class(e1)
    fd2 <- "FunctionalData" %in% class(e2)
    if (!fd1 && fd2) {
      v <- e1
      e1 <- e2
      e2 <- v
    }
    if ("FunctionalData" %in% class(e1)) {
      switch(
        .Generic,
        "+" = e1$copy()$add(e2),
        "-" = e1$copy()$sub(e2),
        "*" = e1$copy()$mul(e2),
        "/" = e1$copy()$div(e2),
        "^" = e1$copy()$pow(e2),
        "%/%" = e1$copy()$div(e2)$apply(round),
        "%%" = e1$copy()$sub(e1$copy()$div(e2)$apply(round)$mul(e2)),
        "==" = e1$copy()$dist(e2) == 0,
        "!=" = e1$copy()$dist(e2) != 0,
        "<" = e1$normdist(e2) < 0,
        "<=" = e1$normdist(e2) <= 0,
        ">" = e1$normdist(e2) > 0,
        ">=" = e1$normdist(e2) >= 0
      )
    }
  }
}
