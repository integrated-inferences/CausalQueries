
#' n_check
#'
#' @param n An integer. Sample size argument.
#'
#' @details Checks whether the input is an integer greater than 0.
#' @keywords internal

n_check <- function(n) {
    cond1 <- !(round(n) == n)
    cond2 <- n < 0
    cond_joint <- cond1 | cond2
    if (cond_joint)
        stop("Number of observation has to be an integer bigger than 0.")
}

#' default_stan_control
#'
#' @param adapt_delta A double between 0 and 1. It determines \code{adapt_delta}
#' @param max_treedepth A positive integer. It determines  \code{maximum_tree_depth}
#' @details Sets controls to default unless otherwise specified.
#' @keywords internal

default_stan_control <- function(adapt_delta = NULL, max_treedepth = 15L) {
    if (is.null(adapt_delta)) {
        adapt_delta <- 0.95
    }
    list(adapt_delta = adapt_delta, max_treedepth = max_treedepth)
}


#' set_sampling_args
#' From 'rstanarm' (November 1st, 2019)
#'
#' @param object A \code{stanfit} object.
#' @param user_dots A list. User commands.
#' @param user_adapt_delta A double between 0 and 1. Adapt delta passed by the user
#' @param ... further arguments to be passed to 'stan'
#' @details Set the sampling arguments
#' @keywords internal

set_sampling_args <- function(object, user_dots = list(), user_adapt_delta = NULL, ...) {
    args <- list(object = object, ...)
    unms <- names(user_dots)
    for (j in seq_along(user_dots)) {
        args[[unms[j]]] <- user_dots[[j]]
    }
    defaults <- default_stan_control(adapt_delta = user_adapt_delta)
    if (!"control" %in% unms) {
        args$control <- defaults
    } else {
        if (!is.null(args$control$user_adapt_delta)) {
            args$control$adapt_delta <- user_adapt_delta
        } else {
            args$control$adapt_delta <- defaults$adapt_delta
        }
        if (is.null(args$control$max_treedepth)) {
            args$control$max_treedepth <- defaults$max_treedepth
        }
    }
    args$save_warmup <- FALSE
    return(args)

}
