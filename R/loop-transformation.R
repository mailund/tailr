# I need to import these for CHECK to work, so I might as well do it here...
#' @importFrom assertthat assert_that
#' @importFrom glue glue
#' @importFrom rlang enquo
# other than satisfying CHECK, I'm not using these imports since I qualify the functions
# by their namespace.


can_call_be_transformed <- function(
                                    call_name, call_arguments,
                                    fun_name, fun_call_allowed, cc) {
    switch(call_name,
        "if" = {
            can_transform_rec(call_arguments[[1]], fun_name, fun_call_allowed, cc)
            can_transform_rec(call_arguments[[2]], fun_name, TRUE, cc)
            can_transform_rec(call_arguments[[3]], fun_name, TRUE, cc)
        },

        "for" = {
            warning("We can't yet handle loops.")
            cc(FALSE)
        },
        "while" = {
            warning("We can't yet handle loops.")
            cc(FALSE)
        },
        "repeat" = {
            warning("We can't yet handle loops.")
            cc(FALSE)
        }, {
            if (call_name == fun_name && !fun_call_allowed) {
                warn_msg <- simpleWarning(
                    "The function cannot be transformed since it contains a recursive call inside a call.",
                    call = NULL
                )
                warning(warn_msg)
                cc(FALSE)
            }
            fun_call_allowed <- FALSE
            for (arg in call_arguments) {
                can_transform_rec(arg, fun_name, fun_call_allowed, cc)
            }
        }
    )
    return(TRUE)
}

can_transform_rec <- function(expr, fun_name, fun_call_allowed, cc) {
    if (rlang::is_atomic(expr) || rlang::is_pairlist(expr) ||
        rlang::is_symbol(expr) || rlang::is_primitive(expr)) {
        return(TRUE)
    } else {
        assertthat::assert_that(rlang::is_call(expr))
        call_name <- rlang::call_name(expr)
        call_arguments <- rlang::call_args(expr)
        can_call_be_transformed(call_name, call_arguments, fun_name, fun_call_allowed, cc)
    }
}

#' Tests if a function, provided by its name, can be transformed.
#'
#' @param fun The function to check. Must be provided by its (bare symbol) name.
#'
#' @examples
#' factorial <- function(n)
#'     if (n <= 1) 1 else n * factorial(n - 1)
#' factorial_acc <- function(n, acc = 1)
#'     if (n <= 1) acc else factorial_acc(n - 1, n * acc)
#' can_transform(factorial) # FALSE
#' can_transform(factorial_acc) # TRUE
#'
#' @export
can_transform <- function(fun) {
    fun <- rlang::enquo(fun)

    fun_name <- rlang::get_expr(fun)
    if (!rlang::is_symbol(fun_name)) {
        error <- simpleError(
            glue::glue(
                "Since we need to recognise recursion, we can only manipulate ",
                "functions provided to can_transform by name.\n",
                "Use a bare symbol."
            ),
            call = match.call()
        )
        stop(error)
    }

    fun <- rlang::eval_tidy(fun)
    if (!rlang::is_closure(fun)) {
        error <- simpleError(
            glue::glue(
                "The function provided to can_transform must be a user-defined function.\n",
                "Instead, it is {fun_name} == {deparse(fun)}."
            ),
            call = match.call()
        )
        stop(error)
    }

    callCC(function(cc) can_transform_rec(body(fun), fun_name, TRUE, cc))
}
