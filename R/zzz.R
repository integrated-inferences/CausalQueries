#' Package attach hook
#'
#' Prints a copy-paste command for enabling parallel Stan chains when
#' \code{mc.cores} is unset. Does not call \code{parallel::detectCores()}.
#' Quiet on non-interactive sessions and when \code{mc.cores} is already set
#' (including PSOCK workers that inherit options).
#'
#' @keywords internal
#' @noRd

.onAttach <- function(libname, pkgname) {
  if (!interactive()) {
    return(invisible(NULL))
  }
  if (!is.null(getOption("mc.cores"))) {
    return(invisible(NULL))
  }
  packageStartupMessage(
    "CausalQueries: For large problems, consider enabling parallel computation.\n",
    "To enable: options(mc.cores = parallel::detectCores())"
  )
  invisible(NULL)
}
