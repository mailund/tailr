
## Test for possibility of transformation #########################################

# I need to import the Depends packagefor CHECK to work, so I might as well do it here...
# Other than satisfying CHECK, I'm not using these imports since I qualify the functions
# by their namespace.

#' Tests if a call object can be transformed.
#'
#' @param call_name Name (function) of the call.
#' @param call_arguments The call's arguments
#' @param fun_name The name of the recursive function we want to transform
#' @param fun_call_allowed Whether a recursive call is allowed at this point
#' @param cc Current continuation to abort if a transformation is not possible
#'
#' @return TRUE, if the expression can be transformed. Invokes \code{cc} otherwise.
can_call_be_transformed <- function(call_name, call_arguments,
                                    fun_name, fun_call_allowed, cc) {
    switch(call_name,
        # Code blocks -- don't consider those calls.
        "{" = {
            for (arg in call_arguments) {
                can_transform_rec(arg, fun_name, fun_call_allowed, cc)
            }
        },

        # Selection
        "if" = {
            can_transform_rec(call_arguments[[1]], fun_name, fun_call_allowed, cc)
            can_transform_rec(call_arguments[[2]], fun_name, TRUE, cc)
            if (length(call_arguments) == 3) {
                can_transform_rec(call_arguments[[3]], fun_name, TRUE, cc)
            }
        },

        # Loops
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
        },

        # All other calls
        {
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

#' Recursive call for testing if an expression can be transformed into a looping tail-recursion.
#'
#' @param expr The expression to test
#' @param fun_name The name of the recursive function we want to transform
#' @param fun_call_allowed Whether a recursive call is allowed at this point
#' @param cc Current continuation, used to escape if the expression cannot be transformed.
#'
#' @return TRUE, if the expression can be transformed. Invokes \code{cc} otherwise.
can_transform_rec <- function(expr, fun_name, fun_call_allowed, cc) {
    if (rlang::is_atomic(expr) || rlang::is_pairlist(expr) ||
        rlang::is_symbol(expr) || rlang::is_primitive(expr)) {
        return(TRUE)
    } else {
        stopifnot(rlang::is_lang(expr))
        call_name <- rlang::call_name(expr)
        call_arguments <- rlang::call_args(expr)
        can_call_be_transformed(call_name, call_arguments, fun_name, fun_call_allowed, cc)
    }
}


#' @describeIn can_loop_transform This version expects \code{fun} to be quosure.
#' @export
can_loop_transform_ <- function(fun) {
    fun_name <- rlang::get_expr(fun)
    if (!rlang::is_symbol(fun_name)) {
        error <- simpleError(
            glue::glue(
                "Since we need to recognise recursion, we can only manipulate ",
                "functions provided to can_loop_transform by name.\n",
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
                "The function provided to can_loop_transform must be a user-defined function.\n",
                "Instead, it is {fun_name} == {deparse(fun)}."
            ),
            call = match.call()
        )
        stop(error)
    }

    ## FIXME: don't transform twice; we do this now both here and in the
    ## loop transformation
    fun_body <- user_transform(body(fun), rlang::get_env(fun))
    callCC(function(cc) can_transform_rec(fun_body, fun_name, TRUE, cc))
}


#' Tests if a function, provided by its name, can be transformed.
#'
#' This function analyses a recursive function to check if we can transform it into
#' a loop or trampoline version with \code{\link{transform}}. Since this function needs to handle
#' recursive functions, it needs to know the name of its input function, so this must be
#' provided as a bare symbol.
#'
#' @param fun The function to check. Must be provided by its (bare symbol) name.
#'
#' @examples
#' factorial <- function(n)
#'     if (n <= 1) 1 else n * factorial(n - 1)
#' factorial_acc <- function(n, acc = 1)
#'     if (n <= 1) acc else factorial_acc(n - 1, n * acc)
#'
#' can_loop_transform(factorial)     # FALSE -- and prints a warning
#' can_loop_transform(factorial_acc) # TRUE
#'
#' can_loop_transform_(rlang::quo(factorial))     # FALSE -- and prints a warning
#' can_loop_transform_(rlang::quo(factorial_acc)) # TRUE
#'
#' @describeIn can_loop_transform This version quotes \code{fun} itself.
#' @export
can_loop_transform <- function(fun) {
    fun <- rlang::enquo(fun)
    can_loop_transform_(fun)
}

## Function transformation ###################################################

#' Make exit points into explicit calls to return.
#'
#' This function dispatches on a call object to set the context of recursive
#' expression modifications.
#'
#' @param call_expr The call to modify.
#' @param in_function_parameter Is the expression part of a parameter to a function call?
#' @return A modified expression.
make_returns_explicit_call <- function(call_expr, in_function_parameter) {
    call_name <- rlang::call_name(call_expr)
    call_args <- rlang::call_args(call_expr)

    switch(call_name,
        # For if-statments we need to treat the condition as in a call
        # but the two branches will have the same context as the enclosing call.
        "if" = {
            call_expr[[2]] <- make_returns_explicit(call_args[[1]], TRUE)
            call_expr[[3]] <- make_returns_explicit(call_args[[2]], in_function_parameter)
            if (length(call_args) == 3) {
                call_expr[[4]] <- make_returns_explicit(call_args[[3]], in_function_parameter)
            }
        },

        # We don't treat blocks as calls and we only transform the last argument
        # of the block. Explicit returns are the only way to exit the block in earlier
        # statements, anyway
        "{" = {
            n <- length(call_expr)
            call_expr[[n]] <- make_returns_explicit(call_expr[[n]], in_function_parameter)
        },

        # For all other calls we transform the arguments inside a call context.
        {
            for (i in seq_along(call_args)) {
                call_expr[[i + 1]] <- make_returns_explicit(call_args[[i]], TRUE)
            }
            if (!in_function_parameter) { # if we weren't parameters, we are a value to be returned
                call_expr <- rlang::call2("return", call_expr)
            }
        }
    )

    call_expr
}

#' Make exit points into explicit calls to return.
#'
#' @param expr An expression to transform
#' @param in_function_parameter Is the expression part of a parameter to a function call?
#' @return A modified expression.
make_returns_explicit <- function(expr, in_function_parameter) {
    if (rlang::is_atomic(expr) || rlang::is_pairlist(expr) ||
        rlang::is_symbol(expr) || rlang::is_primitive(expr)) {
        if (in_function_parameter) {
            expr
        } else {
            rlang::call2("return", expr)
        }
    } else {
        stopifnot(rlang::is_lang(expr))
        make_returns_explicit_call(expr, in_function_parameter)
    }
}

#' Translate a return(<recursive-function-call>) expressions into
#' a block that assigns the parameters to local variables and call `continue`.
#'
#' @param recursive_call The call object where we get the parameters
#' @param fun The actual function -- we use this for the call to `match.call`.
#' @return The rewritten expression
translate_recursive_call_into_next <- function(recursive_call, fun) {
    expanded_call <- match.call(definition = fun, call = recursive_call)
    assignments <- as.list(expanded_call)[-1]
    variables <- names(assignments)

    # We cannot do a simple loop and assign expressions to the local variables.
    # If we did, the order of assignments might matter. We need a parallel
    # assignment.
    new_vars <- vector("character", length = length(assignments))
    new_assignments <- vector("list", length = length(assignments))
    for (i in seq_along(assignments)) {
        new_vars[[i]] <- paste0("..", variables[[i]])
        new_assignments[[i]] <-
            call(
                "<-",
                rlang::sym(new_vars[[i]]),
                assignments[[i]]
            )
    }
    for (i in seq_along(assignments)) {
        assignments[[i]] <-
            call(
                "<-",
                rlang::sym(variables[[i]]),
                rlang::sym(new_vars[[i]])
            )
    }

    as.call(c(rlang::sym("{"), new_assignments, assignments, `next`))
}

#' Translate all return(<recursive-function-call>) expressions into
#' a block that assigns the parameters to local variables.
#'
#' @param expr The expression to rewrite
#' @param fun_name The name of the recursive function we are rewriting
#' @param fun The actual function -- we use this for the call to `match.call`.
#' @return The rewritten expression
transform_recursive_calls <- function(expr, fun_name, fun) {
    if (rlang::is_atomic(expr) || rlang::is_pairlist(expr) ||
        rlang::is_symbol(expr) || rlang::is_primitive(expr)) {
        expr
    } else {
        stopifnot(rlang::is_lang(expr))
        call_name <- rlang::call_name(expr)
        if (call_name == "return") {
            if (rlang::is_lang(expr[[2]])) {
                call_name <- rlang::call_name(expr[[2]])
                if (call_name == fun_name) {
                    return(translate_recursive_call_into_next(expr[[2]], fun))
                }
            }
        }
        expr_args <- rlang::call_args(expr)
        for (i in seq_along(expr_args)) {
            expr[[i + 1]] <- transform_recursive_calls(expr_args[[i]], fun_name, fun)
        }
        expr
    }
}


#' Simplify nested code-blocks.
#'
#' If a call is \code{\{} and has a single expression inside it, replace it with that expression.
#'
#' @param expr The expression to rewrite
#' @return The new expression
simplify_nested_blocks <- function(expr) {
    if (rlang::is_atomic(expr) || rlang::is_pairlist(expr) ||
        rlang::is_symbol(expr) || rlang::is_primitive(expr)) {
        expr
    } else {
        stopifnot(rlang::is_lang(expr))
        call_name <- rlang::call_name(expr)
        if (call_name == "{" && length(expr) == 2) {
            simplify_nested_blocks(expr[[2]])
        } else {
            args <- rlang::call_args(expr)
            for (i in seq_along(args)) {
                expr[[i + 1]] <- simplify_nested_blocks(args[[i]])
            }
            expr
        }
    }
}

#' Construct the expression for a transformed function body.
#'
#' This is where the loop-transformation is done. This function translates
#' the body of a recursive function into a looping function.
#'
#' @param fun_expr The original function body.
#' @param fun_name The name of the recursive function.
#' @param fun The actual function definition.
#'
#' @return The body of the transformed function.
build_transformed_function <- function(fun_expr, fun_name, fun) {
    fun_expr <- make_returns_explicit(fun_expr, FALSE)
    fun_expr <- transform_recursive_calls(fun_expr, fun_name, fun)
    fun_expr <- simplify_nested_blocks(fun_expr)
    rlang::call2("repeat", fun_expr)
}

#' Transform a function from recursive to looping.
#'
#' Since this function needs to handle recursive functions, it needs to know the
#' name of its input function, so this must be provided as a bare symbol.
#'
#' @param fun The function to transform. Must be provided as a bare name.
#'
#' @export
loop_transform <- function(fun) {
    fun_q <- rlang::enquo(fun)
    fun <- rlang::eval_tidy(fun)
    if (!can_loop_transform_(fun_q)) {
        warning("Could not build a transformed function")
        return(fun)
    }

    fun_name <- rlang::quo_name(fun_q)
    user_transformed_body <- user_transform(body(fun), rlang::get_env(fun_q))
    new_fun_body <- build_transformed_function(body(fun), fun_name, fun)
    rlang::new_function(
        args = formals(fun),
        body = new_fun_body,
        env = rlang::get_env(fun_q)
    )
}
