
# This is a dummy. It is only here to prevent notes from package checks where
# we would otherwise get a complaint that `escape` is not defined. Everywhere
# when we actually call a `escape` function, it is a continuation from calls to
# `callCC`.
escape <- identity # nocov
`!<-` <- function(x, value) value  # nocov -- also to silence notes

## Test for possibility of transformation #########################################


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

check_can_call_be_transformed <- function(call_name, call_arguments,
                                          fun_name, fun_call_allowed,
                                          escape) {
    # compute if a call is allowed -- which is what we return for the topdown
    # parameter for other callbacks -- and abort using escape if
    # we see a call when it is not allowed.
    switch(call_name,
        # Code blocks -- don't consider those calls.
        "{" = return(fun_call_allowed),

        # Explicit returns are not considered calls either.
        "return" = return(fun_call_allowed),

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
                call = rlang::expr(eval(!!!call_arguments))
            )
            warning(msg)
            return(fun_call_allowed) # get out, and hope the user knows what he is doing...
        },

        # With expressions are a bit like eval, I guess... don't consider them
        # function calls.
        "with" = return(fun_call_allowed),

        # Selection
        "if" = return(fun_call_allowed),

        # Loops
        "for" = {
            warning("We can't yet handle loops.")
            escape(FALSE)
        },
        "while" = {
            warning("We can't yet handle loops.")
            escape(FALSE)
        },
        "repeat" = {
            warning("We can't yet handle loops.")
            escape(FALSE)
        },

        # All other calls
        {
            if (call_name == fun_name && !fun_call_allowed) {
                warn_msg <- simpleWarning(
                    "The function cannot be transformed since it contains a recursive call inside a call.",
                    call = NULL
                )
                warning(warn_msg)
                escape(FALSE)
            }
            return(FALSE) # inside calls we do not allow recursive calls
        }
    )
    stop("we shouldn't reach this point!") # nocov
}


try_loop_transform <- function(fun) {
    fun_name <- as.character(rlang::get_expr(fun))
    # fun_env <- rlang::get_env(fun)
    fun <- rlang::eval_tidy(fun)

    check_call_callback <- function(expr, escape, topdown, ...) {
        call_name <- rlang::call_name(expr)
        call_arguments <- rlang::call_args(expr)
        fun_call_allowed <- topdown
        check_can_call_be_transformed(call_name, call_arguments, fun_name, fun_call_allowed, escape)
    }
    callCC(
        function(escape) {
            fun %>%
                foolbox::rewrite_with(
                    foolbox::rewrite_callbacks() %>%
                        foolbox::with_topdown_call_callback(check_call_callback),
                    escape = escape,
                    topdown = TRUE # topdown is check for whether call is allowed
                )
        }
    )
}

#' @describeIn can_loop_transform This version expects \code{fun} to be quosure.
#' @import foolbox
#' @export
can_loop_transform_ <- function(fun) {
    check_function_argument(fun)
    transformed <- fun %>% try_loop_transform()
    # if we get a function, we can transform
    if (is.logical(transformed)) transformed else TRUE
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
#' can_loop_transform(factorial) # FALSE -- and prints a warning
#' can_loop_transform(factorial_acc) # TRUE
#'
#' can_loop_transform_(rlang::quo(factorial)) # FALSE -- and prints a warning
#' can_loop_transform_(rlang::quo(factorial_acc)) # TRUE
#' @describeIn can_loop_transform This version quotes \code{fun} itself.
#' @export
can_loop_transform <- function(fun) {
    can_loop_transform_(rlang::enquo(fun))
}

## Function transformation ###################################################

## Ensure returns are explicit #####

# the rules for when to insert returns is such that it is easier to do the recursion
# explicitly than use foolbox. Essentially, the top-down information should be different
# for different parts of calls, and foolbox doesn't have handles for that.
make_returns_explicit_call <- function(call_expr, in_function_parameter) {
    call_name <- rlang::call_name(call_expr)
    call_args <- rlang::call_args(call_expr)

    switch(call_name,
        # For if-statments we need to treat the condition as in a call
        # but the two branches will have the same context as the enclosing call.
        "if" = {
            call_expr[[2]] <- make_returns_explicit_expr(call_args[[1]], TRUE)
            call_expr[[3]] <- make_returns_explicit_expr(call_args[[2]], in_function_parameter)
            if (length(call_args) == 3) {
                call_expr[[4]] <- make_returns_explicit_expr(call_args[[3]], in_function_parameter)
            }
        },

        # We don't treat blocks as calls and we only transform the last argument
        # of the block. Explicit returns are the only way to exit the block in earlier
        # statements, anyway
        "{" = {
            n <- length(call_expr)
            call_expr[[n]] <- make_returns_explicit_expr(call_expr[[n]], in_function_parameter)
        },

        # Not sure how to handle eval, exactly...
        # The problem here is that I need to return the expression if it is not a recursive call
        # but not if it is... the check gives a warning, so here we just do nothing
        "eval" = {
            # do nothing
        },

        # With should just be left alone and we can deal with the expression it evaluates
        "with" = {
            call_expr[[3]] <- make_returns_explicit_expr(call_expr[[3]], in_function_parameter)
        },

        # For all other calls we transform the arguments inside a call context.
        {
            for (i in seq_along(call_args)) {
                call_expr[[i + 1]] <- make_returns_explicit_expr(call_args[[i]], TRUE)
            }
            if (!in_function_parameter) { # if we weren't parameters, we are a value to be returned
                call_expr <- rlang::expr(return(!!call_expr))
            }
        }
    )

    call_expr
}

make_returns_explicit_expr <- function(expr, in_function_parameter) {
    if (rlang::is_atomic(expr) || rlang::is_pairlist(expr) ||
        rlang::is_symbol(expr) || rlang::is_primitive(expr)) {
        if (in_function_parameter) {
            expr
        } else {
            rlang::expr(return(!!expr))
        }
    } else {
        stopifnot(rlang::is_lang(expr))
        make_returns_explicit_call(expr, in_function_parameter)
    }
}


make_returns_explicit <- function(fn) {
    body(fn) <- make_returns_explicit_expr(body(fn), FALSE)
    fn
}

## Simplify returns #####
simplify_returns <- function(fn) {
    simplify_callback <- function(expr, ...) {
        if (rlang::is_lang(expr[[2]]) && rlang::call_name(expr[[2]]) == "return") {
              expr[[2]]
          } else {
              expr
          }
    }
    fn %>% rewrite_with(
        rewrite_callbacks() %>% add_call_callback(`return`, simplify_callback)
    )
}

## Translate returns into calls to escape continuation ##########
translate_recursive_call <- function(recursive_call, info) {
    expanded_call <- match.call(definition = info$fun, call = recursive_call)
    arguments <- as.list(expanded_call)[-1]
    vars <- names(arguments)
    tmp_assignments <- vector("list", length = length(arguments))
    for (i in seq_along(arguments)) {
        tmp_var <- parse(text = paste(".tailr_", vars[i], sep = ""))[[1]]
        tmp_assignments[[i]] <- rlang::expr(!!tmp_var <<- !!arguments[[i]])
    }
    as.call(c(
        rlang::sym("{"),
        tmp_assignments
    ))
}

handle_recursive_returns <- function(fn, fun_name) {
    return_callback <- function(expr, ...) {
        if (rlang::is_lang(expr[[2]]) && rlang::call_name(expr[[2]]) == fun_name) {
              translate_recursive_call(expr[[2]], list(fun = fn))
          } else {
              expr
          }
    }
    fn %>% rewrite_with(
        rewrite_callbacks() %>% add_call_callback(`return`, return_callback)
    )
}

returns_to_escapes <- function(fn) {
    return_callback <- function(expr, ...) rlang::expr(escape(!!expr[[2]]))
    fn %>% rewrite_with(
        rewrite_callbacks() %>% add_call_callback(`return`, return_callback)
    )
}

#' Construct the expression for a transformed function body.
#'
#' This is where the loop-transformation is done. This function translates
#' the body of a recursive function into a looping function.
#'
#' @param fun The original function
#' @param fun_name The name of the function we are transforming
#' @return The body of the transformed function.
build_transformed_function <- function(fun, fun_name) {
    fun <- fun %>%
        make_returns_explicit() %>%
        simplify_returns() %>%
        handle_recursive_returns(fun_name) %>%
        returns_to_escapes()

    # wrap everything in a new body...
    vars <- names(formals(fun))
    tmp_assignments <- vector("list", length = length(vars))
    locals_assignments <- vector("list", length = length(vars))
    for (i in seq_along(vars)) {
        local_var <- as.symbol(vars[[i]])
        tmp_var <- parse(text = paste(".tailr_", vars[[i]], sep = ""))[[1]]
        tmp_assignments[[i]] <- rlang::expr(!!tmp_var <- !!local_var)
        locals_assignments[[i]] <- rlang::expr(!!local_var <- !!tmp_var)
    }

    repeat_body <- as.call(
        c(`{`, locals_assignments, body(fun), quote(next))
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
#' @param set_srcref   Flag specifying whether the "srcref" attribute should be
#'                     set to the original value. If you do this, you can print
#'                     the modified function and it will look like the original,
#'                     but printing it will not show the actual, tranformed, source.
#'
#' @export
loop_transform <- function(fun, byte_compile = TRUE, set_srcref = TRUE) {
    fun_q <- rlang::enquo(fun)
    check_function_argument(fun_q)

    fn <- fun_q %>% try_loop_transform()
    if (is.logical(fn)) {
        stopifnot(fn == FALSE)
        warning("Could not build a transformed function")
        return(fun)
    }

    new_fun_body <- build_transformed_function(fn, rlang::get_expr(fun_q))
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
    if (set_srcref) {
        attr(result, "srcref") <- attr(fun, "srcref")
    }

    result
}
