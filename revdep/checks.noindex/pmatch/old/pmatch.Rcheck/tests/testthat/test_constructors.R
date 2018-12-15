context("Constructors")

test_that("We can create constant constructors", {
    type := A | B

    expect_true(exists("A", environment()))
    expect_true(inherits(A, "type"))
    expect_true(exists("B", environment()))
    expect_true(inherits(B, "type"))

    expect_false(rlang::is_null(attr(A, "constructor_constant")))
    expect_equal(attr(A, "constructor_constant"), "A")
})

test_that("We can create function constructors", {
    linked_list := NIL | CONS(car, cdr:linked_list)

    expect_true(exists("NIL", environment()))
    expect_true(inherits(NIL, "linked_list"))

    expect_true(exists("CONS", environment()))
    expect_true(inherits(CONS, "constructor"))

    expect_s3_class(NIL, "linked_list")
    expect_s3_class(CONS(1, NIL), "linked_list")
    expect_s3_class(CONS(1, CONS(2, NIL)), "linked_list")
    expect_error(CONS(1, 2))
})

test_that("We can create print constructed values", {
    linked_list := NIL | CONS(car, cdr:linked_list)

    expect_equal(toString(NIL), "NIL")
    expect_equal(toString(CONS(1, NIL)), "CONS(car = 1, cdr = NIL)")

    expect_output(print(NIL), "NIL")
    expect_output(print(CONS(1, NIL)), "CONS\\(car = 1, cdr = NIL\\)")
})

test_that("We handle syntax errors gracefully", {
    expect_error(
        42 := foo,
        regexp = "Incorrect type specification: 42\\. The type must be a bare symbol."
    )
    expect_error(
        type := 12,
        regexp = paste0(
            "The constructor is malformed.\n",
            "Constructors must either be constanst, i.e. bare symbols, or in the form of a function call."
        )
    )
    expect_error(
        type := f(12),
        regexp = paste0(
            "The constructor argument is malformed.\n",
            "The expression 12 should either be a bare symbol or on the form 'variable : type'."
        )
    )
    expect_error(
        type := f(g(x)),
        regexp = paste0(
            "The constructor argument is malformed.\n",
            "The expression .* should either be a bare symbol or on the form 'variable : type'."
        )
    )
})
