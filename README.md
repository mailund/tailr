
<!-- README.md is generated from README.Rmd. Please edit that file -->

# tailr – Tail recursion optimisations for R programming

[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](http://www.repostatus.org/badges/latest/active.svg)](http://www.repostatus.org/#active)
[![Last-changedate](https://img.shields.io/badge/last%20change-2018--02--10-orange.svg)](/commits/master)
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
    if (n <= 1) acc
    else factorial(n - 1, acc * n)
}
```

We can then, automatically, translate that into a looping version:

``` r
tr_loop_factorial <- tailr::loop_transform(factorial)
tr_loop_factorial
#> function (n, acc = 1) 
#> {
#>     .tailr_env <- rlang::get_env()
#>     .tailr_frame <- rlang::current_frame()
#>     repeat {
#>         if (n <= 1) 
#>             rlang::return_from(.tailr_frame, acc)
#>         else {
#>             rlang::env_bind(.tailr_env, n = n - 1, acc = acc * 
#>                 n)
#>             rlang::return_to(.tailr_frame)
#>         }
#>     }
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
#>                  expr        min         lq        mean      median
#>          factorial(n)     53.166     55.697     87.8910     70.4805
#>     loop_factorial(n)      4.935      5.273     26.5116      6.0450
#>  tr_loop_factorial(n) 123102.498 127133.018 137981.7610 131420.2170
#>           uq        max neval
#>      82.2595   1702.904   100
#>       6.5275   2040.272   100
#>  137787.8420 268219.932   100
```

There is **massive** overhead in the translated version. I believe this
is the parallel assignments that replace the recursive call, see Issue
\#7. Simpler translations were faster. I’m working on improving this\!
