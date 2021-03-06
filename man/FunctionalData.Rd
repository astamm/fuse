% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/FunctionalData.R, R/BayesData.R,
%   R/SobolevData.R
\docType{class}
\name{FunctionalData}
\alias{FunctionalData}
\alias{BayesData}
\alias{SobolevData}
\title{\code{R6} Class for Functional Data}
\format{An \code{\link[R6]{R6Class}} generator object for generating
  \code{\link{FunctionalData}} objects.}
\usage{
FunctionalData$new(grid = c(-5, 5), value = dnorm, resolution = 100L)

BayesData$new(grid = c(-5, 5), value = dnorm, resolution = 100L)

SobolevData$new(grid = c(-5, 5), value = dnorm, resolution = 100L)
}
\value{
A \code{\link{FunctionalData}} object with a set of specific methods
  for functional data.
}
\description{
\code{R6} Class for Functional Data
}
\section{Fields}{

\describe{
\item{\code{range}}{Stores the grid range over which the functional datum is
defined.}
}}

\section{Methods}{
 \describe{ \item{\code{new(grid = c(0, 2 * pi), value =
  sin, resolution = 1000L)}}{This method is used to create object of this
  class with \code{serveraddress} as address of the server object is
  connecting to.} \item{\code{format(...)}}{This method changes server that
  you are contacting with to \code{serveraddress}.}
  \item{\code{evaluate(x)}}{This method creates new session on the server
  with optionally given name in \code{sessionname}.}
  \item{\code{plot()}}{This method changes currently used session on the
  server to the one with id given in \code{sessionid} parameter.} }
}

\examples{
f <- SobolevData$new(value = dnorm, grid = c(-5, 5))
g <- BayesData$new(value = dnorm, grid = c(-5, 5))
f
g
f$plot()
g$plot()
f$add(f)
g$add(g)
f$add(f)$plot()
f$add()$plot()
f$mul(2)$plot()
g$add(g)$plot()
g$mul(2)$plot()
g$mul(1/2)$plot()
g$div(2)$plot()
f$mul(f)$plot()
g$mul(g)$plot()
f$prod(f$mul(2))
f$dist(f$mul(2))
f$dist(f$mul(2), normalize = TRUE)

}
\keyword{data}
\keyword{datasets}
