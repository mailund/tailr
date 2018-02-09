
#' Transform a call before the tail-recursion transformation.
#'
#' This generic function is a hook by which you can modify how the
#' tail-recursion transformations should handle special functions.
#' It gives you a way to rewrite function calls to make them tail-recursive
#' before we do any other manipulation.
#'
#' @param fun   The actual function. Used for dynamic dispatching.
#' @param expr  The expression to rewrite.
#' @return The rewritten expression.
#'
#' @examples
#' my_if_else <- function(test, if_true, if_false) {
#'     if (test) if_true else if_false
#' }
#' f <- function(x, y) my_if_else(x == y, x, f(y, y))
#' f(1, 2)
#' f(3, 3)
#'
#' can_loop_transform(f) # No, we can't, and we get a warning
#'
#' class(my_if_else) <- c("my_if_else", class(my_if_else))
#' class(my_if_else)
#'
#' transform_call.my_if_else <- function(fun, expr) {
#'     test <- expr[[2]]; if_true <- expr[[3]]; if_false <- expr[[4]]
#'     rlang::expr(if (rlang::UQ(test)) rlang::UQ(if_true) else rlang::UQ(if_false))
#' }
#' transform_call(my_if_else, quote(my_if_else(x == y, x, f(y, y))))
#'
#' can_loop_transform(f) # Now we can, because my_if_else gets transformed
#'
#' @export
transform_call <- function(fun, expr) {
    UseMethod("transform_call")
}

#' @describeIn transform_call The default is to just return the unchanged expression.
#' @export
transform_call.default <- function(fun, expr) {
    expr
}

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
#' user_transform_rec(body(f))
user_transform_rec <- function(expr, env = rlang::caller_env()) {
    if (rlang::is_atomic(expr) || rlang::is_pairlist(expr) ||
        rlang::is_symbol(expr) || rlang::is_primitive(expr)) {
        expr
    } else {
        stopifnot(rlang::is_lang(expr))

        fun_name <- rlang::call_name(expr)
        if (!exists(fun_name, where = env)) {
            error_msg <- glue::glue(
                "The function {fun_name} was not found in the provided scope."
            )
            stop(simpleError(error_msg, call = expr))
        }

        fun <- get(fun_name, envir = env)
        args <- rlang::call_args(expr)
        for (i in seq_along(args)) {
            expr[[i + 1]] <- user_transform(args[[i]], env)
        }
        transform_call(fun, expr)
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
