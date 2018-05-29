
<!-- README.md is generated from README.Rmd. Please edit that file -->

## Overview of the `fuse` package

This package provides `R6` classes for describing functional data with
embedding in various infinite-dimensional Hilbert spaces. Currently,
functional data classes are implemented in Sobolev spaces (including the
popular \(L^2\) space of square-integrable functions) and in Bayes
space. Operators `+`, `-`, `*`, and `/` as well as inner product and
induced distance are implemented as well.

## Installation

You can install `fuse` from github with:

``` r
# install.packages("devtools")
devtools::install_github("astamm/fuse")
```

## Example

``` r
library(fuse)
```

We can for example use the default constructor for `FunctionalData`
objects to generate a `FunctionalData` version of the density of the
standard normal distribution. If we create it embedded in Sobolev spaces
and multiply it by `2`, we obtain the following function:

``` r
f <- SobolevData$new()
(2 * f)$plot()
```

![](README-sobolev-1.png)<!-- -->

This is essentially the original density pointwise multiplied by a
factor of `2` which does not result in a density anymore. If instead we
embed the density of the standard normal distribution in Bayes space and
again multiply it by `2`, we get:

``` r
g <- BayesData$new()
(2 * g)$plot()
```

![](README-bayes-1.png)<!-- -->

This shows a fundamental difference between Sobolev and Bayes spaces. In
the latter, adding twice the information content in `g` provides
additional information and thus has the effect of diminishing the
variance of the resulting distribution which turns out to be another
centered normal distribution with smaller variance.
