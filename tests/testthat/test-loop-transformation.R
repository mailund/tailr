context("test-loop-transformation.R")

test_that("we can identify functions we can transform", {
    factorial <- function(n)
        if (n <= 1) 1 else n * factorial(n - 1)
    factorial_acc <- function(n, acc = 1)
        if (n <= 1) acc else factorial_acc(n - 1, n * acc)

    expect_warning(
        expect_false(can_transform(factorial))
    )
    expect_true(can_transform(factorial_acc))
})

test_that("We report errors gracefully", {
    expect_error(
        can_transform(function(x) x),
        "Since we need .*"
    )
    expect_error(
        can_transform(sum),
        "The function provided .*"
    )

    g <- function(x) for (e in x) e
    expect_warning(
        expect_false(tailr::can_transform(g)),
        "We can't .*"
    )

    g <- function(x) repeat {
            TRUE
        }
    expect_warning(
        expect_false(tailr::can_transform(g)),
        "We can't .*"
    )

    g <- function(x) while (TRUE) {
            TRUE
        }
    expect_warning(
        expect_false(tailr::can_transform(g)),
        "We can't .*"
    )
})

test_that("we can transform a simple function", {
    factorial_acc <- function(n, acc = 1)
        if (n <= 1) acc else factorial_acc(n - 1, n * acc)

    transformed <- transform(factorial_acc)

    for (i in 1:10) {
        expect_equal(factorial_acc(i), transformed(i))
    }
})
