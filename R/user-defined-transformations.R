
apply_user_transform <- function(expr, params, env, ...) {
    call_name <- as.character(expr[[1]])

    # Check if we are looking at a local function.
    # as.character will return a vector for qualified names. Those,
    # we simply do not consider local. So we only check for those
    # with length 1.
    if (length(call_name) == 1) {
        if (call_name %in% names(params)) return(expr) # function is a parameter
        if (call_name %in% attr(expr, "bound")) return(expr) # local function
    }

    # now try to get the actual function by evaluating it
    err_fun <- function(e) NULL
    fun <- tryCatch(eval(expr[[1]], env), error = err_fun)
    if (is.null(fun)) return(expr) # don't know the function right now... (why?)

    transformer <- attr(fun, "tailr_transform")
    if (!rlang::is_null(transformer)) transformer(expr) else expr
}


#' Apply user transformations depths-first.
#'
#' @param fn The actual function to transform.
#' @return Rewritten expression
#'
#' @export
user_transform <- function(fn) {
    fn %>% foolbox::rewrite() %>%
        foolbox::rewrite_with(
            foolbox::rewrite_callbacks() %>%
                foolbox::with_call_callback(apply_user_transform)
        )
}
