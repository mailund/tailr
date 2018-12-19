
<!-- README.md is generated from README.Rmd. Please edit that file -->

# tailr – Tail recursion optimisations for R programming

[![Licence](https://img.shields.io/badge/licence-GPL--3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0.en.html)
[![lifecycle](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](http://www.repostatus.org/badges/latest/active.svg)](http://www.repostatus.org/#active)
[![Last-changedate](https://img.shields.io/badge/last%20change-2018--12--19-green.svg)](/commits/master)
[![packageversion](https://img.shields.io/badge/Package%20version-0.1.2.9000-green.svg?style=flat-square)](commits/master)
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

Consider a classical recursive function, `factorial`:

``` r
factorial <- function(n) {
    if (n <= 1) 1
    else n * factorial(n - 1)
}
```

(I know R already has a builtin factorial function, but please ignore
that). This function will compute the factorial of `n`, but if `n` is
too large, it will exceed the stack limit:

``` r
> factorial(3000)
Error: C stack usage  7970184 is too close to the limit
```

A classical way out of this problem is to turn it into a tail-recursive
function:

``` r
factorial <- function(n, acc = 1) {
    if (n <= 1) acc
    else factorial(n - 1, acc * n)
}
```

R doesn’t implement the tail-recursion optimisation, though, so it
doesn’t help us.

``` r
> factorial(3000)
Error: C stack usage  7970184 is too close to the limit
```

With `tailr` we can, automatically, translate a tail-recursive function
into a looping one, essentially implementing the tail-recursion
optimisation this way.

``` r
tr_factorial <- tailr::loop_transform(factorial, byte_compile = FALSE)
```

I have disabled byte compilation to make running time comparisons fair
below; by default it is enabled. For a function as simple as
`factorial`, though, byte compiling will not affect the running time in
any substantial amount.

This version, because it looks instead of recurse, doesn’t have the
stack limit problem:

``` r
tr_factorial(3000)
#> [1] Inf
```

We get the result `Inf` because the number we compute is too large to
represent on the computer, but that is not the point of the example. The
point is that the recursion doesn’t get too deep for the stack because
we avoid recursion alltogether.

With something as simple as computing the factorial, it is easy to write
a looping function by hand, and it will be much faster than both the
(tail-)recursive and the transformed function:

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
                                     tr_factorial(n), 
                                     loop_factorial(n))
bm
#> Unit: microseconds
#>               expr     min       lq      mean   median       uq      max
#>       factorial(n) 510.170 548.6400 692.13391 593.0185 662.8700 5015.846
#>    tr_factorial(n) 650.143 675.7145 741.53741 705.3390 732.8235 2867.557
#>  loop_factorial(n)  47.601  48.6105  76.86726  49.2710  52.7560 2637.968
#>  neval
#>    100
#>    100
#>    100
boxplot(bm)
```

<img src="man/figures/README-unnamed-chunk-5-1.png" width="100%" />

The transformed version runs in about the same time as the recursive
one, but the looping function is much faster.

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
llength <- case_func(acc = 0,
   NIL -> acc,
   CONS(car, cdr) -> llength(cdr, acc + 1)
)
```

``` r
tr_llength <- tailr::loop_transform(llength)
```

The function we generate is rather complicated

``` r
body(tr_llength)
#> .Primitive("{")(.tailr_.match_expr <- .match_expr, .tailr_acc <- acc, 
#>     callCC(function(escape) {
#>         repeat {
#>             .Primitive("{")(.match_expr <- .tailr_.match_expr, 
#>                 acc <- .tailr_acc, if (is.na(.match_expr) && 
#>                   attr(.match_expr, "constructor") == "NIL") 
#>                   escape(acc)
#>                 else if ({
#>                   .cons <- attr(.match_expr, "constructor")
#>                   !is.null(.cons) && .cons == "CONS"
#>                 } && {
#>                   car <- .match_expr$car
#>                   TRUE
#>                 } && {
#>                   cdr <- .match_expr$cdr
#>                   TRUE
#>                 }) {
#>                   .tailr_.match_expr <<- cdr
#>                   .tailr_acc <<- acc + 1
#>                 }
#>                 else {
#>                   escape(stop("None of the patterns match."))
#>                 }, next)
#>         }
#>     }))
```

but, then, it is not one we want to manually inspect in any case.

It is not too hard to implement this function with a loop either, but it
is not as simple as the recursive function:

``` r
is_nil <- case_func(NIL -> TRUE, otherwise -> FALSE)
loop_llength <- function(llist) {
    len <- 0
    while (!is_nil(llist)) {
        len <- len + 1
        llist <- llist$cdr
    }
    len
}
```

If we compare the running time for these three functions, the
transformed function is faster than the recursive but not as fast as the
iterative:

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
                                     tr_llength(test_llist),
                                     loop_llength(test_llist))
bm
#> Unit: microseconds
#>                      expr     min       lq     mean   median       uq
#>       llength(test_llist) 270.776 285.2205 384.5874 300.1905 325.4885
#>    tr_llength(test_llist) 357.037 374.4325 418.4112 387.9800 412.7885
#>  loop_llength(test_llist) 144.618 154.0980 221.8522 160.4235 175.0040
#>       max neval
#>  4886.636   100
#>  2318.699   100
#>  5466.565   100
boxplot(bm)
```

<img src="man/figures/README-unnamed-chunk-11-1.png" width="100%" />

**FIXME:** The main reason for this is that the running time is
dominated by the cost of `pmatch`. If I manage to get that faster, the
iterative function might end being much faster here as well.

More examples:

``` r
llcontains <- case_func(x,
    NIL -> FALSE,
    CONS(car, cdr) -> if (car == x) TRUE else llcontains(cdr, x)
)

tr_llcontains <- tailr::loop_transform(llcontains)

loop_contains <- function(lst, x) {
    while (!is_nil(lst)) {
        if (x == lst$car) return(TRUE)
        else lst <- lst$cdr
    }
}

lst <- make_llist(100)
bm <- microbenchmark::microbenchmark(llcontains(lst, 1001),
                                     tr_llcontains(lst, 1001),
                                     loop_contains(lst, 1001))
bm
#> Unit: microseconds
#>                      expr     min       lq     mean   median       uq
#>     llcontains(lst, 1001) 259.903 262.8160 343.9029 269.3025 276.7305
#>  tr_llcontains(lst, 1001) 362.543 369.0980 412.5989 372.7685 388.0770
#>  loop_contains(lst, 1001) 199.942 204.3275 238.1532 209.8665 216.5210
#>       max neval
#>  4959.340   100
#>  3285.773   100
#>  2746.243   100
boxplot(bm)
```

<img src="man/figures/README-unnamed-chunk-12-1.png" width="100%" />

``` r
llrev <- pmatch::case_func(acc = NIL,
    NIL -> acc,
    CONS(car, cdr) -> llrev(cdr, CONS(car, acc))
)

bubble <- case_func(swapped = FALSE, acc = NIL,
    CONS(first, CONS(second, rest)) -> 
        if (first > second) bubble(CONS(first, rest), TRUE, CONS(second, acc))
        else bubble(CONS(second, rest), swapped, CONS(first, acc)),
    CONS(x, NIL) -> list(new_list = llrev(CONS(x, acc)), swapped = swapped)
)

bubble_sort <- function(lst) {
    if (is_nil(lst)) return(lst)
    bind[lst, swapped] <- bubble(lst)
    while (swapped) {
        bind[lst, swapped] <- bubble(lst)
    }
    lst
}

lst <- CONS(3, CONS(2, CONS(5, CONS(1, NIL))))
bubble_sort(lst)
#> CONS(car = 1, cdr = CONS(car = 2, cdr = CONS(car = 3, cdr = CONS(car = 5, cdr = NIL))))
```

``` r
tr_llrev <- pmatch::case_func(acc = NIL,
    NIL -> acc,
    CONS(car, cdr) -> llrev(cdr, CONS(car, acc))
)

tr_llrev <- tailr::loop_transform(tr_llrev)

tr_bubble <- case_func(swapped = FALSE, acc = NIL,
    CONS(first, CONS(second, rest)) -> 
        if (first > second) tr_bubble(CONS(first, rest), TRUE, CONS(second, acc))
        else tr_bubble(CONS(second, rest), swapped, CONS(first, acc)),
    CONS(x, NIL) -> list(new_list = tr_llrev(CONS(x, acc)), swapped = swapped)
)
tr_bubble <- tailr::loop_transform(tr_bubble)

tr_bubble_sort <- function(lst) {
    if (is_nil(lst)) return(lst)
    bind[lst, swapped] <- tr_bubble(lst)
    while (swapped) {
        bind[lst, swapped] <- tr_bubble(lst)
    }
    lst
}

lst <- CONS(3, CONS(2, CONS(5, CONS(1, NIL))))
tr_bubble_sort(lst)
#> CONS(car = 1, cdr = CONS(car = 2, cdr = CONS(car = 3, cdr = CONS(car = 5, cdr = NIL))))
```

``` r
loop_llrev <- function(lst) {
    acc <- NIL
    while (!is_nil(lst)) {
        acc <- CONS(lst$car, acc)
        lst <- lst$cdr
    }
    acc
}
loop_bubble <- function(lst, swapped = FALSE) {
    acc <- NIL
    repeat {
        if (is_nil(lst$cdr)) 
            return(list(new_list = loop_llrev(CONS(lst$car, acc)),
                        swapped = swapped))
        
        first <- lst$car
        second <- lst$cdr$car
        rest <- lst$cdr$cdr
        if (first > second) {
            acc <- CONS(second, acc)
            lst <- CONS(first, rest)
            swapped <- TRUE
        } else {
            acc <- CONS(first, acc)
            lst <- CONS(second, rest)
        }
    }
}

loop_bubble_sort <- function(lst) {
    if (is_nil(lst)) return(lst)
    bind[lst, swapped] <- loop_bubble(lst)
    while (swapped) {
        bind[lst, swapped] <- loop_bubble(lst)
    }
    lst
}

lst <- CONS(3, CONS(2, CONS(5, CONS(1, NIL))))
loop_bubble_sort(lst)
#> CONS(car = 1, cdr = CONS(car = 2, cdr = CONS(car = 3, cdr = CONS(car = 5, cdr = NIL))))
```

``` r
lst <- make_llist(10)
bm <- microbenchmark::microbenchmark(bubble_sort(lst),
                                     tr_bubble_sort(lst),
                                     loop_bubble(lst))
bm
#> Unit: microseconds
#>                 expr      min        lq      mean   median        uq
#>     bubble_sort(lst) 3466.611 3808.9600 4222.5333 3971.868 4139.7010
#>  tr_bubble_sort(lst) 3823.465 4192.0190 4526.8456 4325.548 4535.4200
#>     loop_bubble(lst)  104.267  120.0635  129.5123  126.008  136.0245
#>        max neval
#>  12112.025   100
#>   7323.989   100
#>    188.959   100
boxplot(bm)
```

<img src="man/figures/README-unnamed-chunk-16-1.png" width="100%" />

The module primarily solves the problem of exceeding the stack space.
The transformed functions are not as fast as those we can code by hand
using loops. It *should* be possible to improve on the running time of
the transformed functions, however, with some program analysis… This
analysis should be included in the time usage analysis, though, which
will probably still come out saying that manually programmed looping
versions are faster than transformed functions. Recursive functions can
be a lot easier to read, though, than their corresponding looping
versions, especially with pattern matching.
