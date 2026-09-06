#' Shared catalogue of model component names for inspect / summary / print
#'
#' Single source of truth so \code{inspect}/\code{grab}, \code{summary.causal_model},
#' and \code{print.summary.causal_model} cannot drift apart on supported
#' \code{what} / \code{include} names.
#'
#' @return A list with:
#'   \itemize{
#'     \item \code{on_demand}: large objects only attached when requested via
#'       \code{summary(..., include = )} / \code{inspect}'s large set
#'     \item \code{all}: every supported component name
#'   }
#' @keywords internal
#' @noRd
model_component_catalogue <- function() {
  on_demand <- c(
    "parameter_mapping",
    "parameter_matrix",
    "causal_types",
    "prior_event_probabilities",
    "prior_distribution",
    "ambiguities_matrix",
    "type_prior"
  )

  # Always available after summary() (or from stan_objects / model slots)
  always <- c(
    "statement",
    "nodes",
    "parents",
    "parents_df",
    "parameters",
    "parameters_df",
    "parameter_names",
    "nodal_types",
    "data_types",
    "prior_hyperparameters",
    "type_posterior",
    "posterior_distribution",
    "posterior_event_probabilities",
    "data",
    "stan_summary",
    "stanfit",
    "stan_warnings"
  )

  list(
    on_demand = on_demand,
    always = always,
    all = unique(c(always, on_demand))
  )
}

#' Stop if requested component names are not in the catalogue
#' @keywords internal
#' @noRd
check_model_component_names <- function(requested, catalogue = model_component_catalogue()) {
  if (is.null(requested) || !length(requested)) {
    return(invisible(NULL))
  }
  wrong <- base::setdiff(requested, catalogue$all)
  if (length(wrong) > 0L) {
    stop(
      "The following requested objects are not supported: ",
      paste0(wrong, collapse = ", "),
      ".\nAvailable objects are: ",
      paste(catalogue$all, collapse = ", "),
      call. = FALSE
    )
  }
  invisible(NULL)
}
