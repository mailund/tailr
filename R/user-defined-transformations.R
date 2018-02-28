
#' Apply user transformations depths-first.
#'
#' The difference between this function and \code{\link{user_transform}} is that the this
#' function does not perform type checks before calling recursively while \code{\link{user_transform}}
#' does.
#'
#' @param expr The expression to transform -- typically a function body.
#' @param env  The environment where functions can be found.
#'
#' @return Rewritten expression
user_transform_rec <- function(expr, env = rlang::caller_env()) {
    if (rlang::is_atomic(expr) || rlang::is_pairlist(expr) ||
        rlang::is_symbol(expr) || rlang::is_primitive(expr)) {
        expr
    } else {
        stopifnot(rlang::is_lang(expr))

        # see if we can figure out which function we are dealing with...
        fun <- tryCatch(rlang::call_fn(expr), error = function(e) NULL)
        if (is.null(fun)) {
            fun_name <- rlang::call_name(expr)
            if (exists(fun_name, env)) {
                fun <- get(fun_name, envir = env)
            } else {
                error_msg <- glue::glue(
                    "The function {fun_name} was not found in the provided scope."
                )
                stop(simpleError(error_msg, call = expr))
            }
        }

        args <- rlang::call_args(expr)
        for (i in seq_along(args)) {
            expr[[i + 1]] <- user_transform(args[[i]], env)
        }
        if (!rlang::is_null(transformer <- attr(fun, "tailr_transform"))) {
            expr <- transformer(expr)
        }
        expr
    }
}

#' Apply user transformations depths-first.
#'
#' @param expr The expression to transform -- typically a function body.
#' @param env  The environment where functions can be found.
#'
#' @return Rewritten expression
#'
#' @examples
#' my_if_else <- function(test, if_true, if_false) {
#'     if (test) if_true else if_false
#' }
#' class(my_if_else) <- c("my_if_else", class(my_if_else))
#' transform_call.my_if_else <- function(fun, expr) {
#'     test <- expr[[2]]; if_true <- expr[[3]]; if_false <- expr[[4]]
#'     rlang::expr(if (rlang::UQ(test)) rlang::UQ(if_true) else rlang::UQ(if_false))
#' }
#'
#' f <- function(x, y) my_if_else(x == y, x, f(y, y))
#' user_transform(body(f))
#'
#' @export
user_transform <- function(expr, env = rlang::caller_env()) {
    if (!rlang::is_expression(expr)) {
        error_msg <- glue::glue(
            "The `expr' argument is not a quoted expression.\n",
            "We expect to get an expression to transform, usually the body of a function.\n",
            "Did you by any chance pass the function as argument instead of its body?\n"
        )
        stop(simpleError(error_msg))
    }
    user_transform_rec(expr, env)
}
