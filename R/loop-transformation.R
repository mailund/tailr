
# This is a dummy. It is only here to prevent notes from package checks where
# we would otherwise get a complaint that `escape` is not defined. Everywhere
# when we actually call a `escape` function, it is a continuation from calls to
# `callCC`.
escape <- function(x) x # nocov

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

        # Explicit returns are not considered calls either.
        "return" = {
            for (arg in call_arguments) {
                can_transform_rec(arg, fun_name, fun_call_allowed, cc)
            }
        },

        # Eval is really just evaluation of an expression in the calling scope,
        # so we shouldn't consider those function calls either... I'm not sure
        # how to handle them when it comes to what they return, though, since it
        # depends on the expression they will evaluate

        "eval" = {
            msg <- simpleWarning(
                glue::glue(
                    "This function contains an eval-expression. It is hard to work out if those ",
                    "are tail-recursive, so such expressions are not analysed and left alone in ",
                    "transformations."
                ),
                call = rlang::expr(eval(!!! call_arguments))
            )
            warning(msg)
            return(TRUE) # get out, and hope the user knows what he is doing...
        },

        # With expressions are a bit like eval, I guess... don't consider them
        # function calls.
        "with" = {
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

#' Recursive call for testing if an expression can be transformed into a looping
#' tail-recursion.
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


check_function_argument <- function(fun) {
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
}

#' @describeIn can_loop_transform This version expects \code{fun_body} to be both tested
#'                                and user-transformed.
#'
#' @param fun_name Name of the recursive function.
#' @param fun_body The user-transformed function body.
#' @param env      Environment used to look up variables used in \code{fun_body}.
#'
#' @export
can_loop_transform_body <- function(fun_name, fun_body, fun, env) {
    fun_body <- user_transform(fun_body, fun, env)
    callCC(function(cc) can_transform_rec(fun_body, fun_name, TRUE, cc))
}

#' @describeIn can_loop_transform This version expects \code{fun} to be quosure.
#' @export
can_loop_transform_ <- function(fun) {
    check_function_argument(fun)

    fun_name <- rlang::get_expr(fun)
    fun_env <- rlang::get_env(fun)
    fun <- rlang::eval_tidy(fun)

    can_loop_transform_body(fun_name, body(fun), fun, fun_env)
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
    can_loop_transform_(rlang::enquo(fun))
}

## Function transformation ###################################################

#' Make exit points into explicit calls to return.
#'
#' This function dispatches on a call object to set the context of recursive
#' expression modifications.
#'
#' @param call_expr The call to modify.
#' @param in_function_parameter Is the expression part of a parameter to a function call?
#' @param info  Information passed along with transformations.
#' @return A modified expression.
make_returns_explicit_call <- function(call_expr, in_function_parameter, info) {
    call_name <- rlang::call_name(call_expr)
    call_args <- rlang::call_args(call_expr)

    switch(call_name,
        # For if-statments we need to treat the condition as in a call
        # but the two branches will have the same context as the enclosing call.
        "if" = {
            call_expr[[2]] <- make_returns_explicit(call_args[[1]], TRUE, info)
            call_expr[[3]] <- make_returns_explicit(call_args[[2]], in_function_parameter, info)
            if (length(call_args) == 3) {
                call_expr[[4]] <- make_returns_explicit(call_args[[3]], in_function_parameter, info)
            }
        },

        # We don't treat blocks as calls and we only transform the last argument
        # of the block. Explicit returns are the only way to exit the block in earlier
        # statements, anyway
        "{" = {
            n <- length(call_expr)
            call_expr[[n]] <- make_returns_explicit(call_expr[[n]], in_function_parameter, info)
        },

        # Not sure how to handle eval, exactly...
        # The problem here is that I need to return the expression if it is not a recursive call
        # but not if it is... the check gives a warning, so here we just do nothing
        "eval" = {
            # do nothing
        },

        # With should just be left alone and we can deal with the expression it evaluates
        "with" = {
            call_expr[[3]] <- make_returns_explicit(call_expr[[3]], in_function_parameter, info)
        },

        # For all other calls we transform the arguments inside a call context.
        {
            for (i in seq_along(call_args)) {
                call_expr[[i + 1]] <- make_returns_explicit(call_args[[i]], TRUE, info)
            }
            if (!in_function_parameter) { # if we weren't parameters, we are a value to be returned
                call_expr <- rlang::expr(return(!! call_expr))
            }
        }
    )

    call_expr
}

#' Make exit points into explicit calls to return.
#'
#' @param expr An expression to transform
#' @param in_function_parameter Is the expression part of a parameter to a function call?
#' @param info Information passed along the transformations.
#' @return A modified expression.
make_returns_explicit <- function(expr, in_function_parameter, info) {
    if (rlang::is_atomic(expr) || rlang::is_pairlist(expr) ||
        rlang::is_symbol(expr) || rlang::is_primitive(expr)) {
        if (in_function_parameter) {
            expr
        } else {
            rlang::expr(return(!! expr))
        }
    } else {
        stopifnot(rlang::is_lang(expr))
        make_returns_explicit_call(expr, in_function_parameter, info)
    }
}

#' Removes return(return(...)) cases.
#'
#' This function dispatches on a call object to set the context of recursive
#' expression modifications.
#'
#' @param call_expr The call to modify.
#' @param info  Information passed along with transformations.
#' @return A modified expression.
simplify_returns_call <- function(call_expr, info) {
    call_name <- rlang::call_name(call_expr)
    call_args <- rlang::call_args(call_expr)

    switch(call_name,
        # Handle returns
        "return" = {
            call_expr[[2]] <- simplify_returns(call_args[[1]], info)
            if (rlang::is_lang(call_expr[[2]]) && rlang::call_name(call_expr[[2]]) == "return") {
                call_expr <- call_expr[[2]]
            }
        },

        # For all other calls we transform the arguments inside a call context.
        {
            for (i in seq_along(call_args)) {
                call_expr[[i + 1]] <- simplify_returns(call_args[[i]], info)
            }
        }
    )

    call_expr
}

#' Remove return(return(...)) expressions
#'
#' @param expr An expression to transform
#' @param info Information passed along the transformations.
#' @return A modified expression.
simplify_returns <- function(expr, info) {
    if (rlang::is_atomic(expr) || rlang::is_pairlist(expr) ||
        rlang::is_symbol(expr) || rlang::is_primitive(expr)) {
        expr
    } else {
        stopifnot(rlang::is_lang(expr))
        simplify_returns_call(expr, info)
    }
}

#' Translate a return(<recursive-function-call>) expressions into
#' a block that assigns the parameters to local variables and call `next`.
#'
#' @param recursive_call The call object where we get the parameters
#' @param info           Information passed along to the transformations.
#' @return The rewritten expression
translate_recursive_call <- function(recursive_call, info) {
    expanded_call <- match.call(definition = info$fun, call = recursive_call)
    arguments <- as.list(expanded_call)[-1]
    vars <- names(arguments)
    tmp_assignments <- vector("list", length = length(arguments))
    for (i in seq_along(arguments)) {
        tmp_var <- parse(text = paste(".tailr_", vars[i], sep = ""))[[1]]
        tmp_assignments[[i]] <- rlang::expr(rlang::UQ(tmp_var) <<- rlang::UQ(arguments[[i]]))
    }
    as.call(c(
        rlang::sym("{"),
        tmp_assignments
    ))
}

#' Handles the actual recursive returns
#'
#' This function dispatches on a call object to set the context of recursive
#' expression modifications.
#'
#' @param call_expr The call to modify.
#' @param info  Information passed along with transformations.
#' @return A modified expression.
handle_recursive_returns_call <- function(call_expr, info) {
    call_name <- rlang::call_name(call_expr)
    call_args <- rlang::call_args(call_expr)

    switch(call_name,
        # Handle returns
        "return" = {
            call_expr[[2]] <- handle_recursive_returns(call_args[[1]], info)
            if (rlang::is_lang(call_expr[[2]]) && rlang::call_name(call_expr[[2]]) == info$fun_name) {
                call_expr <- translate_recursive_call(call_expr[[2]], info)
            }
        },

        # For all other calls we just recurse
        {
            for (i in seq_along(call_args)) {
                call_expr[[i + 1]] <- handle_recursive_returns(call_args[[i]], info)
            }
        }
    )

    call_expr
}

#' Handle the actual recursive calls
#'
#' @param expr An expression to transform
#' @param info Information passed along the transformations.
#' @return A modified expression.
handle_recursive_returns <- function(expr, info) {
    if (rlang::is_atomic(expr) || rlang::is_pairlist(expr) ||
        rlang::is_symbol(expr) || rlang::is_primitive(expr)) {
        expr
    } else {
        stopifnot(rlang::is_lang(expr))
        handle_recursive_returns_call(expr, info)
    }
}

#' Make calls to return into calls to escapes.
#'
#' This function dispatches on a call object to set the context of recursive
#' expression modifications.
#'
#' @param call_expr The call to modify.
#' @param info  Information passed along with transformations.
#' @return A modified expression.
returns_to_escapes_call <- function(call_expr, info) {
    call_name <- rlang::call_name(call_expr)
    call_args <- rlang::call_args(call_expr)

    switch(call_name,
        # Handle returns
        "return" = {
            call_expr <- rlang::expr(escape(rlang::UQ(call_expr[[2]])))
        },

        # For all other calls we just recurse
        {
            for (i in seq_along(call_args)) {
                call_expr[[i + 1]] <- returns_to_escapes(call_args[[i]], info)
            }
        }
    )

    call_expr
}

#' Make calls to return into calls to escapes.
#'
#' @param expr An expression to transform
#' @param info Information passed along the transformations.
#' @return A modified expression.
returns_to_escapes <- function(expr, info) {
    if (rlang::is_atomic(expr) || rlang::is_pairlist(expr) ||
        rlang::is_symbol(expr) || rlang::is_primitive(expr)) {
        expr
    } else {
        stopifnot(rlang::is_lang(expr))
        returns_to_escapes_call(expr, info)
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
#' @param info Information passed along the transformations.
#' @return The body of the transformed function.
build_transformed_function <- function(fun_expr, info) {
    vars <- names(formals(info$fun))
    tmp_assignments <- vector("list", length = length(vars))
    locals_assignments <- vector("list", length = length(vars))
    for (i in seq_along(vars)) {
        local_var <- as.symbol(vars[[i]])
        tmp_var <- parse(text = paste(".tailr_", vars[[i]], sep = ""))[[1]]
        tmp_assignments[[i]] <- rlang::expr(rlang::UQ(tmp_var) <- rlang::UQ(local_var))
        locals_assignments[[i]] <- rlang::expr(rlang::UQ(local_var) <- rlang::UQ(tmp_var))
    }

    # this would be a nice pipeline, but it is a bit much to require
    # magrittr just for this
    fun_expr <- make_returns_explicit(fun_expr, FALSE, info)
    fun_expr <- simplify_returns(fun_expr, info)
    fun_expr <- handle_recursive_returns(fun_expr, info)
    fun_expr <- returns_to_escapes(fun_expr, info)
    fun_expr <- simplify_nested_blocks(fun_expr)

    repeat_body <- as.call(
        c(`{`, locals_assignments, fun_expr, quote(next))
    )
    call_cc_stmt <- rlang::expr(
        callCC(function(escape) {
            repeat {
                !!repeat_body
            }
        })
    )
    as.call(
        c(`{`, tmp_assignments, call_cc_stmt)
    )
}

#' Transform a function from recursive to looping.
#'
#' Since this function needs to handle recursive functions, it needs to know the
#' name of its input function, so this must be provided as a bare symbol.
#'
#' @param fun          The function to transform. Must be provided as a bare name.
#' @param byte_compile Flag specifying whether to compile the function after
#'                     transformation.
#'
#' @export
loop_transform <- function(fun, byte_compile = TRUE) {
    fun_q <- rlang::enquo(fun)
    check_function_argument(fun_q)

    fun <- rlang::eval_tidy(fun)
    fun_name <- rlang::get_expr(fun_q)
    fun_env <- rlang::get_env(fun_q)
    fun_body <- user_transform(body(fun), fun, fun_env)


    if (!can_loop_transform_body(fun_name, fun_body, fun, fun_env)) {
        warning("Could not build a transformed function")
        return(fun)
    }
    info <- list(fun = fun, fun_name = fun_name)

    new_fun_body <- build_transformed_function(fun_body, info)
    result <- rlang::new_function(
        args = formals(fun),
        body = new_fun_body,
        env = rlang::get_env(fun_q)
    )
    if (byte_compile) {
        if (!requireNamespace("compiler")) { # nocov start
            msg <- simpleWarning(
                glue::glue(
                    "The compiler package is not installed, so the ",
                    "function will not be byte-compiled.\n",
                    "To disable this warning, install the package, or ",
                    "set the flag byte_compile to FALSE."
                )
            )
            warning(msg)
            return(result)
        } # nocov end
        result <- compiler::cmpfun(result)
    }
    attr(result, "srcref") <- attr(fun, "srcref")

    result
}
