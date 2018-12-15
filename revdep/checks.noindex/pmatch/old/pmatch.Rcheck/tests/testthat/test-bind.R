context("Local variable bindings")

test_that("simple bindings works", {
    bind[x] <- 2
    expect_equal(x, 2)

    bind[x, y] <- 4:5
    expect_equal(x, 4)
    expect_equal(y, 5)
})

test_that("bindings work with pattern matching", {
    llist := NIL | CONS(car, cdr:llist)

    l <- CONS(1, CONS(2, CONS(3, NIL)))

    bind[CONS(head, rest)] <- l
    expect_equal(head, 1)

    bind[CONS(first, CONS(second, rest)), CONS(first_2, rest)] <- list(l, rest)
    expect_equal(first, 1)
    expect_equal(second, 2)
    expect_equal(first_2, 2)
})

test_that("pattern matching provide good error messages", {
    llist := NIL | CONS(car, cdr:llist)

    expect_error(
        bind[CONS(head, rest)] <- NIL,
        "The pattern .* does not match its value."
    )
})
