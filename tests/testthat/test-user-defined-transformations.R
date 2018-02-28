context("test-user-defined-transformations.R")

test_that("we call the default transformation function", {
    f <- function(x) x
    transformed_f_body <- user_transform(body(f))
    expect_equal(body(f), transformed_f_body)
})

test_that("we transform functions with user-defined re-writing rules", {
    my_if_else <- function(test, if_true, if_false) {
        if (test) if_true else if_false
    }
    my_if_else_transform <- function(expr) {
        test <- expr[[2]]
        if_true <- expr[[3]]
        if_false <- expr[[4]]
        rlang::expr(if (rlang::UQ(test)) rlang::UQ(if_true) else rlang::UQ(if_false))
    }
    attr(my_if_else, "tailr_transform") <- my_if_else_transform

    f <- function(x, y) my_if_else(x == y, x, f(y, y))
    transformed_body <- user_transform(body(f))
    expect_equal(transformed_body, quote(if (x == y) x else f(y, y)))
})

test_that("we handle errors", {
    f <- function(x) g(x)

    expect_error(
        user_transform(f),
        regexp = "The `expr' argument is not a quoted expression.*"
    )

    expect_error(
        user_transform(body(f)),
        regexp = "The function g was not found in the provided scope."
    )
})
