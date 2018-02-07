
<!-- README.md is generated from README.Rmd. Please edit that file -->

# tailr – Tail recursion optimisations for R programming

[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](http://www.repostatus.org/badges/latest/active.svg)](http://www.repostatus.org/#active)
[![Last-changedate](https://img.shields.io/badge/last%20change-2018--02--07-orange.svg)](/commits/master)
[![lifecycle](http://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![Licence](https://img.shields.io/badge/licence-GPL--3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0.en.html)

[![Travis build
status](https://travis-ci.org/mailund/tailr.svg?branch=master)](https://travis-ci.org/mailund/tailr)
[![Appveyor build
status](https://ci.appveyor.com/api/projects/status/1d36yh8klursko82/branch/master?svg=true)](https://ci.appveyor.com/project/mailund/tailr/branch/master)
[![Coverage
status](https://codecov.io/gh/mailund/tailr/branch/master/graph/badge.svg)](https://codecov.io/github/mailund/tailr?branch=master)
[![Coverage
status](http://coveralls.io/repos/github/mailund/tailr/badge.svg?branch=master)](https://coveralls.io/github/mailund/tailr?branch=master)

[![minimal R
version](https://img.shields.io/badge/R%3E%3D-3.1-blue.svg)](https://cran.r-project.org/)
[![packageversion](https://img.shields.io/badge/Package%20version-0.0.0.9000-orange.svg?style=flat-square)](commits/master)
<!--[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/tailr)](https://cran.r-project.org/package=tailr)
-->

-----

The goal of tailr is to automatically transform tail-recursive functions
into loops or trampolines.

## Installation

You can install tailr from GitHub with:

``` r
# install.packages("devtools")
devtools::install_github("mailund/tailr")
```

## Examples

We can take a classical recursive function and write it in a
tail-recursive form using an accumulator:

``` r
factorial <- function(n, acc = 1) {
    if (n <= 1) { acc }
    else { factorial(n - 1, acc * n) }
}
```

We can then, automatically, translate that into a looping version:

``` r
tr_loop_factorial <- tailr::loop_transform(factorial)
tr_loop_factorial
#> function (n, acc = 1) 
#> repeat if (n <= 1) return(acc) else {
#>     ..n <- n - 1
#>     ..acc <- acc * n
#>     n <- ..n
#>     acc <- ..acc
#>     .Primitive("next")
#> }
```

We can then compare the running time with the recursive function and a
version that is written using a loop:

``` r
loop_factorial <- function(n) {
    val <- 1
    while (n > 1) {
        val <- n * val
        n <- n - 1
    }
    val
}


n <- 100
microbenchmark::microbenchmark(factorial(n), loop_factorial(n), tr_loop_factorial(n))
#> Unit: microseconds
#>                  expr    min      lq     mean  median      uq      max
#>          factorial(n) 54.160 54.7920 78.04577 55.4230 57.0595 2010.211
#>     loop_factorial(n)  4.895  5.0455 29.81842  5.1555  5.3450 2456.944
#>  tr_loop_factorial(n)  9.263  9.8165 34.28456 10.3640 10.7810 2326.050
#>  neval
#>    100
#>    100
#>    100
```

There is some overhead in the translated version. I believe this is the
parallel assignments that replace the recursive call, see Issue \#7.
