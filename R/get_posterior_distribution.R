#' Get the posterior distribution from a model
#'
#' Access the posterior distribution from the model if one has been added via `update_model`.
#'
#' @inheritParams CausalQueries_internal_inherit_params
#' @keywords internal
#' @return A `data.frame` with parameters draws
#' @family posterior_distribution

get_posterior_distribution <- function(model) {
  .Deprecated("grab")
  if (is.null(model$posterior_distribution)) {
    message("The model does not contain a posterior distribution. A posterior distribution can be provided to the model using `update_model`")
    return(NULL)
  }
  return(model$posterior_distribution)
}

