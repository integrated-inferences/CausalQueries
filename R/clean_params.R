#' Check parameters sum to 1 in param_set; normalize if needed; add
#' names if needed
#'
#' @param parameters_df A \code{data.frame}. This object is first generated by
#'   \code{\link{make_model}}.
#' @param warning Logical. Whether to print warning (if any) in console.
#'   Defaults to TRUE
#' @return A parameters \code{data.frame}  with names and where parameters
#'   sum to 1.
#' @noRd
#' @keywords internal
#' @examples
#' \donttest{
#' model <- make_model('X->Y')
#' model$parameters_df$param_value <- 1:6
#' CausalQueries:::clean_params(model$parameters_df, warning = TRUE)
#'}

clean_params <- function(parameters_df, warning = TRUE) {
  if (nrow(parameters_df) == 0) {
    message("parameters_df is empty")
    return(parameters_df)
  }

  # Check positive
  if (min(parameters_df$priors) < 0) {
    stop("Negative alpha arguments for priors are not allowed")
  }

  # Check positive
  if (min(parameters_df$param_value) < 0) {
    stop("Negative arguments for parameters not allowed")
  }

  # Normalize parameters if needed
  for (j in unique(parameters_df$param_set)) {
    A <- parameters_df$param_set == j
    check <- sum(parameters_df$param_value[A])

    if (!isTRUE(all.equal(check, 1))) {
      if (warning) {
        message(paste0(
          "Parameters in set ",
          j,
          " do not sum to 1. Using normalized parameters"
        ))
      }
      parameters_df$param_value[A] <- parameters_df$param_value[A] /
        check
    }
  }

  return(parameters_df)
}



#' Clean parameter vector
#'
#' @inheritParams CausalQueries_internal_inherit_params
#' @noRd
#' @keywords internal
#' @return A vector of named parameters summing to 1.
clean_param_vector <- function(model, parameters) {
  parameters <- as.vector(parameters)

  if (!is.atomic(parameters)) {
    stop("parameters should be a vector")
  }

  model$parameters_df$param_value <- parameters
  x <- clean_params(model$parameters_df, warning = FALSE)$param_value
  names(x) <- model$parameters_df$param_names

  return(x)
}
