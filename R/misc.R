
#' n_check
#'
#' @param n An integer. Sample size argument.
#' @return An error message if \code{n} is not a non-negative integer.
#' @details Checks whether the input is a non-negative integer (\code{n = 0}
#'   is allowed).
#' @noRd
#' @keywords internal

n_check <- function(n) {
    if (!is.numeric(n) || length(n) != 1L || is.na(n)) {
      stop("Number of observations has to be a non-negative integer.")
    }
    cond1 <- !(round(n) == n)
    cond2 <- n < 0
    cond_joint <- cond1 | cond2
    if (cond_joint) {
      stop("Number of observations has to be a non-negative integer.")
    }
}

#' Cores for Stan chain parallelism (CRAN-safe)
#'
#' At most 2 when \code{_R_CHECK_LIMIT_CORES_} is set (R CMD check / CRAN
#' policy). Otherwise \code{parallel::detectCores()}.
#'
#' @return Integer >= 1
#' @keywords internal
#' @noRd
stan_cores <- function() {
  limit <- tolower(Sys.getenv("_R_CHECK_LIMIT_CORES_", ""))
  if (nzchar(limit) && !identical(limit, "false")) {
    return(2L)
  }
  n <- suppressWarnings(parallel::detectCores())
  if (length(n) != 1L || is.na(n) || n < 1L) {
    return(1L)
  }
  as.integer(n)
}

#' Set \code{options(mc.cores)} for parallel Stan chains
#'
#' @param quiet If \code{FALSE}, print the value set.
#' @return Integer cores used, invisibly
#' @keywords internal
#' @noRd
enable_stan_parallel <- function(quiet = FALSE) {
  n <- stan_cores()
  options(mc.cores = n)
  if (!isTRUE(quiet)) {
    message(
      "CausalQueries: options(mc.cores = ", n,
      ") for parallel Stan chains"
    )
  }
  invisible(n)
}

#' default_stan_control
#'
#' @param adapt_delta A double between 0 and 1. It determines
#'   \code{adapt_delta}
#' @param max_treedepth A positive integer. It determines
#'   \code{maximum_tree_depth}
#' @details Sets controls to default unless otherwise specified.
#' @return A \code{list} containing arguments to be passed to \code{stan}
#' @noRd
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
#' @param ... further arguments to be passed to 'stan'
#' @details Set the sampling arguments. Values supplied by the user take
#'   precedence; defaults are only used to fill in elements the user has
#'   not specified.
#' @return A \code{list} with arguments to be passed to \code{stan}
#' @noRd
#' @keywords internal

set_sampling_args <- function(object,
                              user_dots = list(), ...) {
    args <- list(object = object, ...)
    unms <- names(user_dots)
    for (j in seq_along(user_dots)) {
        args[[unms[j]]] <- user_dots[[j]]
    }
    defaults <- default_stan_control()
    if (!"control" %in% unms) {
        args$control <- defaults
    } else {
        if (is.null(args$control[["adapt_delta"]])) {
            args$control$adapt_delta <- defaults$adapt_delta
        }
        if (is.null(args$control[["max_treedepth"]])) {
            args$control$max_treedepth <- defaults$max_treedepth
        }
    }
    if (is.null(args$save_warmup)) {
        args$save_warmup <- FALSE
    }
    return(args)
}
