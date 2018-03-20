
<!-- README.md is generated from README.Rmd. Please edit that file -->

# tailr – Tail recursion optimisations for R programming

[![Licence](https://img.shields.io/badge/licence-GPL--3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0.en.html)
[![lifecycle](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](http://www.repostatus.org/badges/latest/active.svg)](http://www.repostatus.org/#active)
[![Last-changedate](https://img.shields.io/badge/last%20change-2018--03--20-green.svg)](/commits/master)
[![packageversion](https://img.shields.io/badge/Package%20version-0.1.1.9002-green.svg?style=flat-square)](commits/master)

[![Travis build
status](https://travis-ci.org/mailund/tailr.svg?branch=master)](https://travis-ci.org/mailund/tailr)
[![Appveyor build
status](https://ci.appveyor.com/api/projects/status/1d36yh8klursko82/branch/master?svg=true)](https://ci.appveyor.com/project/mailund/tailr/branch/master)
[![Coverage
status](https://codecov.io/gh/mailund/tailr/branch/master/graph/badge.svg)](https://codecov.io/github/mailund/tailr?branch=master)
[![Coverage
status](http://coveralls.io/repos/github/mailund/tailr/badge.svg?branch=master)](https://coveralls.io/github/mailund/tailr?branch=master)

[![CRAN
status](http://www.r-pkg.org/badges/version/tailr)](https://cran.r-project.org/package=tailr)
[![CRAN
downloads](http://cranlogs.r-pkg.org/badges/grand-total/tailr)](https://cran.r-project.org/package=tailr)
[![minimal R
version](https://img.shields.io/badge/R-%E2%89%A53.2-blue.svg)](https://cran.r-project.org/)

-----

Recursive functions are the natural way to express iterations in a
functional programming langauge, but in R, they can be significantly
slower than loop-versions and for moderately long sequences or
moderately deep trees, recursive functions will reach a limit imposted
on them by the stack limit.

There are known solutions to these problems, as long as functions are
written to be tail-recursive, meaning that the return value of a
function is either a base value or another recursive call, but where we
do not call recursively to then do something with the result.

The goal of `tailr` is to automatically transform tail-recursive
functions into loops or trampolines.

## Installation

You can install the released version of `tailr` from CRAN using

``` r
install.packages("tailr")
```

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
tr_factorial <- tailr::loop_transform(factorial, byte_compile = FALSE)
tr_factorial
#> function(n, acc = 1) {
#>     if (n <= 1) acc
#>     else factorial(n - 1, acc * n)
#> }

tr_factorial(100)
#> [1] 9.332622e+157
```

I have disabled byte compilation to make running time comparisons fair;
by default it is enabled. For a function as simple as `factorial`,
though, byte compiling will not affect the running time in any
substantial amount.

We can compare the running time with the recursive function and a
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


n <- 1000
bm <- microbenchmark::microbenchmark(factorial(n), 
                                     loop_factorial(n), 
                                     tr_factorial(n))
bm
#> Unit: microseconds
#>               expr     min       lq      mean   median       uq      max
#>       factorial(n) 715.304 772.4265 1035.9802 863.2585 1179.665 6816.073
#>  loop_factorial(n)  51.310  51.8860   96.7987  53.6600   60.598 3491.843
#>    tr_factorial(n) 791.451 821.8215 1100.5864 907.5140 1166.180 2606.855
#>  neval
#>    100
#>    100
#>    100
boxplot(bm)
```

<img src="man/figures/README-unnamed-chunk-3-1.png" width="100%" />

There is *some* overhead in using the automatically translated version
over the hand-written, naturally, and for a simple function such as
`factorial`, it is not hard to write the loop-variant instead of the
recursive function.

However, consider a more complicated example. Using the `pmatch`
package, we can create a linked list data structure as this:

``` r
library(pmatch)
llist := NIL | CONS(car, cdr : llist)
```

A natural way to process linked lists using pattern matching is to write
recursive functions that matches different patterns of their input. A
function for computing the length of a linked list can look like this:

``` r
llength <- function(llist, acc = 0) {
    cases(llist,
          NIL -> acc,
          CONS(car, cdr) -> llength(cdr, acc + 1))
}
```

It is reasonably simple to understand this function, whereas a looping
version is somewhat more complicated. An initial attempt could look like
this:

``` r
loop_llength <- function(llist) {
    acc <- 0
    repeat {
        cases(llist,
              NIL -> return(acc),
              CONS(car, cdr) -> {
                  acc <- acc + 1
                  llist <- cdr
              })
    }
}
```

This version will not function, however, since it tries to `return` from
inside a call to `cases`, and `return` only works inside the immediate
scope.

Instead, we can use `callCC` to implement a non-local return like this:

``` r
loop_llength <- function(llist) {
    callCC(function(escape) {
        acc <- 0
        repeat {
            cases(llist,
                  NIL -> escape(acc),
                  CONS(car, cdr) -> {
                      acc <<- acc + 1
                      llist <<- cdr
                  })
        }    
    })
}
```

Notice that we have to use the `<<-` assignment operator here. This is
for the same reason that we need a non-local return. The expression
inside the call to `cases` is evaluated in a different environment than
the local function environment, so to get to the actual variables we
want to assign to, we need the non-local assignment operator.

It is possible to avoid `cases` using other functions from the `pmatch`
package, but the result is vastly more compliated since pattern matching
and expressions that should be evaluated per case needs to handle
scoping. We can automatically make such a function using `tailr`,
however:

``` r
tr_llength <- tailr::loop_transform(llength)
```

The function we generate is rather complicated

``` r
tr_llength
#> function(llist, acc = 0) {
#>     cases(llist,
#>           NIL -> acc,
#>           CONS(car, cdr) -> llength(cdr, acc + 1))
#> }
#> <bytecode: 0x7fac8a3e9238>
```

but, then, it is not one we want to manually inspect in any case.

The automatically generated function is complicated, but it actually
outcompetes the hand-written loop version.

``` r
make_llist <- function(n) {
    l <- NIL
    for (i in 1:n) {
        l <- CONS(i, l)
    }
    l
}
test_llist <- make_llist(100)
bm <- microbenchmark::microbenchmark(llength(test_llist),
                                     loop_llength(test_llist),
                                     tr_llength(test_llist))
bm
#> Unit: milliseconds
#>                      expr      min       lq     mean   median       uq
#>       llength(test_llist) 54.94337 65.40173 68.42609 67.61124 71.20118
#>  loop_llength(test_llist) 59.18011 70.31971 75.76034 74.02095 78.67105
#>    tr_llength(test_llist) 35.13879 43.11308 45.63485 45.09175 48.55855
#>        max neval
#>   99.00812   100
#>  134.51344   100
#>   60.88526   100
boxplot(bm)
```

<img src="man/figures/README-unnamed-chunk-9-1.png" width="100%" />

It is, of course, possible to write a faster hand-written function to
deal with this case, but it will be about as complicated as the
automatically generated function, and you don’t really want to write
that by hand.

As you have no doubt noticed about `llength`, it is not in fact
tail-recursive, from the look of it, since the final recursion is
enclosed by a call to `cases`. The function is only tail-recursive
because it can be translated into one by rewriting the `cases` function
call to a sequence of `if`-statements. The `tailr` package doesn’t
handle `cases` from `pmatch` by knowing about this package. Instead, it
has a mechanism that lets you provide re-writing rules.

If you set the attribute “tailr\_transform” on a function, and set this
attribute to a function, then that function will be called when `tailr`
sees the function, before it attempts any other processing. The
attribute must be a function that maps an expression to another,
re-written, expression. The one for `cases` looks like this:

``` r
tailr_transform_call <- function(expr) {
    stopifnot(rlang::call_name(expr) == "cases")

    args <- rlang::call_args(expr)
    value <- args[[1]]
    patterns <- args[-1]
    eval(rlang::expr(cases_expr(!!value, !!!patterns)))
}
attr(cases, "tailr_transform") <- tailr_transform_call
```

You can use this mechanism to support tail-recursion for
non-tail-recursive functions that can be rewritten to be tail-recursive.
