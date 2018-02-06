context("test-loop-transformation.R")

test_that("we can identify functions we can transform", {
    factorial <- function(n)
        if (n <= 1) 1 else n * factorial(n - 1)
    factorial_acc <- function(n, acc = 1)
        if (n <= 1) acc else factorial_acc(n - 1, n * acc)


})
