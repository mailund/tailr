context("test-loop-transformation.R")

test_that("we can identify functions we can transform", {
    factorial <- function(n)
        if (n <= 1) 1 else n * factorial(n - 1)
    factorial_acc <- function(n, acc = 1)
        if (n <= 1) acc else factorial_acc(n - 1, n * acc)

    expect_warning(
        expect_false(can_loop_transform(factorial))
    )
    expect_true(can_loop_transform(factorial_acc))
})

test_that("we don't get confused by explicit returns", {
    factorial <- function(n, acc = 1)
        if (n <= 1) return(acc) else return(factorial(n - 1, n * acc))

    expect_true(can_loop_transform(factorial))

    tr_factorial <- loop_transform(factorial)
    for (n in 1:5)
        expect_equal(factorial(n), tr_factorial(n))
})

test_that("We report errors gracefully", {
    expect_error(
        can_loop_transform(function(x) x),
        "Since we need .*"
    )
    expect_error(
        can_loop_transform(sum),
        "The function provided .*"
    )

    g <- function(x) for (e in x) e
    expect_warning(
        expect_false(tailr::can_loop_transform(g)),
        "We can't .*"
    )

    g <- function(x) repeat {
            TRUE
        }
    expect_warning(
        expect_false(tailr::can_loop_transform(g)),
        "We can't .*"
    )

    g <- function(x) while (TRUE) {
            TRUE
        }
    expect_warning(
        expect_false(tailr::can_loop_transform(g)),
        "We can't .*"
    )
})

test_that("we can transform a simple function", {
    factorial_acc <- function(n, acc = 1)
        if (n <= 1) acc else factorial_acc(n - 1, n * acc)
    transformed <- loop_transform(factorial_acc)

    for (i in 1:10) {
        expect_equal(factorial_acc(i), transformed(i))
    }

    # also when the return value is in a call...
    f <- function(x) identity(x)
    transformed <- loop_transform(f)
    for (i in 1:10) {
        expect_equal(f(i), i)
    }

    factorial_acc <- function(n, acc = 1)
        if (n <= 1) identity(acc) else factorial_acc(n - 1, n * acc)
    transformed <- loop_transform(factorial_acc)

    for (i in 1:10) {
        expect_equal(factorial_acc(i), transformed(i))
    }
})

test_that("we cannot transform a non-tail-recursive function", {
    factorial <- function(n, acc = 1)
        if (n <= 1) acc else n * factorial(n - 1)

    expect_warning(
        expect_equal(factorial, loop_transform(factorial)),
        "Could not build .*"
    )
})

test_that("we can handle `with` expressions", {
    f <- function(x) {
        if (x < 0) {
            x
        } else {
            with(list(y = -1), f(x + y))
        }
    }

    expect_true(can_loop_transform(f))
    transformed_f <- loop_transform(f)

    for (x in 1:5) {
        expect_equal(f(x), transformed_f(x))
    }
})

test_that("we warn about eval expressions, but leave them alone", {
    # a valid tail-recurive function
    f <- function(x) if (x < 0) x else eval(quote(f(-1)))
    # an invalid tail-recursive function
    g <- function(x) if (x < 0) x else eval(quote(g(-1) + g(-2)))

    expect_warning(
        can_loop_transform(f),
        "This function contains an eval-expression.*"
    )
    expect_warning(
        can_loop_transform(g),
        "This function contains an eval-expression.*"
    )

    # I'm not sure how to easilly check that eval-expressions are
    # left alone, though...
})

test_that("we handle local varibles when they are functions", {
    cons <- function(car, cdr) list(car = car, cdr = cdr)

    llrev <- function(llist, acc = NULL) {
        if (is.null(llist)) {
              acc
          } else {
              llrev(llist$cdr, list(llist$car, acc))
          }
    }
    llrev <- tailr::loop_transform(llrev)

    llmap <- function(llist, f, acc = NULL) {
        if (is.null(llist)) {
              llrev(acc)
          } else {
              llmap(llist$cdr, f, cons(f(llist$car), acc))
          }
    }

    expect_true(can_loop_transform(llmap))

    llmap <- loop_transform(llmap)

    ll <- cons(1, cons(2, cons(3, NULL)))
    ll2 <- llmap(ll, function(x) 2 * x)

    llength <- function(llist, acc = 0) {
        if (is.null(llist)) {
              acc
          } else {
              llength(llist$cdr, acc + 1)
          }
    }
    llength <- loop_transform(llength)
    as.list.llist <- function(x, all.names = FALSE, sorted = FALSE, ...) {
        n <- llength(x)
        v <- vector("list", length = n)
        i <- 1
        while (i <= n) {
            v[i] <- x$car
            i <- i + 1
            x <- x$cdr
        }
        v
    }
    as.vector.llist <- function(x, mode = "any") {
        unlist(as.list(x))
    }

    expect_equal(as.vector.llist(ll2), c(2, 4, 6))
})
