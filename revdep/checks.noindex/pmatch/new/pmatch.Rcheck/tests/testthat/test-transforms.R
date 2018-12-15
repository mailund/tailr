context("Transformation for eliminating DSL syntax")

test_that("we can translate a function that call cases into one that doesn't", {
    if (!requireNamespace("compiler", quietly = TRUE)) {
        skip("compiler not installed")
    }

    llist := NIL | CONS(car, cdr:llist)
    is_llist_empty <- function(lst, acc = 0)
        cases(
            lst,
            NIL -> TRUE,
            CONS(car, cdr) -> FALSE
        )
    expect_true(is_llist_empty(NIL))
    expect_false(is_llist_empty(CONS(1, NIL)))

    expect_error(compiler::cmpfun(is_llist_empty)) # an error because of the DSL

    is_llist_empty <- transform_cases_function(is_llist_empty)
    is_llist_empty <- compiler::cmpfun(is_llist_empty)
    expect_true(is_llist_empty(NIL))
    expect_false(is_llist_empty(CONS(1, NIL)))

    lldrop <- function(llist, k, acc = NIL) {
        if (k == 0) return(llist)
        pmatch::cases(
            llist,
            NIL -> stop("There were less than k elements in the list"),
            CONS(car, cdr) -> lldrop(cdr, k - 1)
        )
    }
    expect_error(compiler::cmpfun(lldrop)) # an error because of the DSL
    lldrop <- transform_cases_function(lldrop)
    expect_true(cases(
        lldrop(CONS(1, NIL), k = 1),
        NIL -> TRUE, otherwise -> FALSE
    ))
})

test_that("we get appropriate error messages when translating", {
    expect_error(transform_cases_function(1))
    expect_error(transform_cases_function(~.x))
})

## Transformation for the `tailr` package ###################################
context("Transformations for the tailr package")

test_that("we can transform a function that contains a call to cases", {
    if (!requireNamespace("tailr", quietly = TRUE)) {
        skip("tailr not installed")
    }

    llist := NIL | CONS(car, cdr:llist)
    llength <- function(lst, acc = 0)
        cases(
            lst,
            NIL -> acc,
            CONS(car, cdr) -> llength(cdr, acc + 1)
        )

    expect_true(tailr::can_loop_transform(llength))

    llength_tr <- tailr::loop_transform(llength)
    make_llist <- function(n) {
        l <- NIL
        for (i in 1:n) {
            l <- CONS(i, l)
        }
        l
    }

    for (n in 1:10) {
        expect_equal(n, llength_tr(make_llist(n)))
    }
})
