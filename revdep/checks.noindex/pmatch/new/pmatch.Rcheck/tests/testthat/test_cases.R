context("Cases")

test_that("We can match on constant constructors", {
    type := ONE | TWO | THREE
    f <- function(x) cases(x, ONE -> 1, TWO -> 2, THREE -> 3)
    expect_equal(f(ONE), 1)
    expect_equal(f(TWO), 2)
    expect_equal(f(THREE), 3)
    expect_error(f("foo"))

    one <- ONE
    two <- TWO

    expect_null(test_pattern(one, TWO))
    expect_null(test_pattern_(one, quote(TWO)))
    expect_true(!rlang::is_null(test_pattern(two, TWO)))
    expect_true(!rlang::is_null(test_pattern_(two, quote(TWO))))
})

test_that("We can create match on function constructors", {
    linked_list := NIL | CONS(car, cdr:linked_list)

    f <- function(lst) {
        cases(
            lst,
            NIL -> 0,
            CONS(x, NIL) -> 1,
            CONS(x, CONS(y, NIL)) -> 2,
            CONS(x, CONS(y, CONS(z, NIL))) -> 3,
            otherwise -> 42
        )
    }

    expect_equal(f(NIL), 0)
    expect_equal(f(CONS(1, NIL)), 1)
    expect_equal(f(CONS(1, CONS(2, NIL))), 2)
    expect_equal(f(CONS(1, CONS(2, CONS(3, NIL)))), 3)
    expect_equal(f(CONS(1, CONS(2, CONS(3, CONS(4, NIL))))), 42)

    tree := T(left:tree, right:tree) | L(value)
    res <- cases(
        T(L(4), L(5)),
        L(v) -> v,
        T(L(v), L(w)) -> v + w,
        otherwise -> 5
    )
    expect_equal(res, 9)
})

test_that("We can formula as well as assignment syntax", {
    type := ONE | TWO | THREE
    f <- function(x) cases(x, ONE ~ 1, TWO ~ 2, THREE ~ 3)
    f <- function(x) cases(x, ONE -> 1, TWO -> 2, THREE -> 3)
    expect_equal(f(ONE), 1)
    expect_equal(f(TWO), 2)
    expect_equal(f(THREE), 3)
    expect_error(f("foo"))

    one <- ONE
    two <- TWO

    expect_null(test_pattern(one, TWO))
    expect_null(test_pattern_(one, quote(TWO)))
    expect_true(!rlang::is_null(test_pattern(two, TWO)))
    expect_true(!rlang::is_null(test_pattern_(two, quote(TWO))))

    linked_list := NIL | CONS(car, cdr:linked_list)

    f <- function(lst) {
        cases(
            lst,
            NIL ~ 0,
            CONS(x, NIL) ~ 1,
            CONS(x, CONS(y, NIL)) ~ 2,
            CONS(x, CONS(y, CONS(z, NIL))) ~ 3,
            otherwise ~ 42
        )
    }

    expect_equal(f(NIL), 0)
    expect_equal(f(CONS(1, NIL)), 1)
    expect_equal(f(CONS(1, CONS(2, NIL))), 2)
    expect_equal(f(CONS(1, CONS(2, CONS(3, NIL)))), 3)
    expect_equal(f(CONS(1, CONS(2, CONS(3, CONS(4, NIL))))), 42)

    tree := T(left:tree, right:tree) | L(value)
    res <- cases(
        T(L(4), L(5)),
        L(v) ~ v,
        T(L(v), L(w)) ~ v + w,
        otherwise ~ 5
    )
    expect_equal(res, 9)
})


test_that("We can create match constants on function constructors", {
    linked_list := NIL | CONS(car, cdr:linked_list)

    f <- function(lst) {
        cases(
            lst,
            NIL -> 0,
            CONS(1, NIL) -> 11,
            CONS(2, NIL) -> 22,
            CONS(x, NIL) -> x,
            otherwise -> 42
        )
    }

    expect_equal(f(NIL), 0)
    expect_equal(f(CONS(1, NIL)), 11)
    expect_equal(f(CONS(2, NIL)), 22)
    expect_equal(f(CONS(3, NIL)), 3)
    expect_equal(f(CONS(1, CONS(2, NIL))), 42)
    expect_equal(f(CONS(1, CONS(2, CONS(3, NIL)))), 42)
    expect_equal(f(CONS(1, CONS(2, CONS(3, CONS(4, NIL))))), 42)
})


test_that("We can create match variables in function constructors", {
    linked_list := NIL | CONS(car, cdr:linked_list)

    f <- function(lst) {
        cases(
            lst,
            NIL -> 0,
            CONS(x, NIL) -> x,
            CONS(x, CONS(y, NIL)) -> x + y,
            CONS(x, CONS(y, CONS(z, NIL))) -> x + y + z,
            otherwise -> 42
        )
    }

    expect_equal(f(CONS(1, NIL)), 1)
    expect_equal(f(CONS(11, NIL)), 11)
    expect_equal(f(CONS(1, CONS(2, NIL))), 1 + 2)
    expect_equal(f(CONS(1, CONS(2, CONS(3, NIL)))), 1 + 2 + 3)
})


test_that("We can distinguish between constructors", {
    type := ONE(val) | TWO(val)

    f <- function(x) cases(x, ONE(v) -> 1, TWO(v) -> 2)

    expect_equal(f(ONE(1)), 1)
    expect_equal(f(TWO(1)), 2)
})


test_that("We do not confuse variables for constructors", {
    type := ONE | TWO(x)

    one <- ONE
    two <- TWO(2)

    expect_equal(cases(ONE, ONE -> 1, TWO(two) -> two), 1)
    expect_equal(cases(TWO(3), ONE -> 1, TWO(two) -> two), 3)
    expect_equal(cases(TWO(3), ONE -> 1, x -> x), TWO(3))
    expect_equal(cases(TWO(3), ONE -> 1, two -> two), TWO(3))
    expect_equal(cases(one, ONE -> 1, two -> two), 1)
    expect_equal(cases(two, ONE -> 1, two -> two), two)
    expect_equal(cases(one, one -> one), one)
    expect_equal(cases(one, two -> two), one)
})


test_that("We can do quasi-quoting", {
    type := ONE | TWO(x)
    two_q <- rlang::expr(TWO(2))

    expect_equal(cases(TWO(2), ONE -> 1, rlang::UQ(two_q) -> TWO(2)), TWO(2))
})

test_that("We handle syntax errors gracefully", {
    type := ONE | TWO

    expect_error(
        cases(ONE, x),
        "Malformed matching rule. Rules must be on the form 'pattern -> expression'."
    )
    expect_error(
        cases(TWO, ONE -> 1),
        "None of the patterns matched the expression."
    )

    expect_error(
        cases_expr(ONE),
        "At least one pattern must be provided."
    )
})


test_that("We can build a function using cases_expr", {
    type := ZERO | ONE(x) | TWO(x, y)
    fun_body <- cases_expr(
        v,
        ZERO -> 0,
        ONE(x) -> x,
        TWO(x, y) -> x + y
    )
    fun <- rlang::new_function(alist(v = ), fun_body)

    expect_equal(fun(ZERO), 0)
    expect_equal(fun(ONE(12)), 12)
    expect_equal(fun(TWO(21, 21)), 42)
})



test_that("We can match on more than one pattern", {
    llist := NIL | CONS(car, cdr)
    l1 <- CONS(1, NIL)
    l2 <- CONS(2, l1)
    expect_error(
        cases(l1, ..(NIL, NIL) -> FALSE),
        "When matching against \\.\\. .*"
    )
    expect_error(
        cases(..(l1), ..(NIL, NIL) -> FALSE),
        "When matching against \\.\\. .*"
    )

    expect_true(cases(
        ..(NIL, NIL),
        ..(NIL, NIL) -> TRUE,
        otherwise -> FALSE
    ))
    expect_equal(
        cases(
            ..(l1, l2),
            ..(CONS(x, .), CONS(y, .)) -> x + y,
            otherwise -> FALSE
        ),
        1 + 2
    )
})
