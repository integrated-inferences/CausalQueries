#' Get
#'
#' Returns specified elements from a \code{causal_model}.
#' Users can use \code{get} to extract model's components including
#' nodal types, causal types, parameter priors, parameter posteriors,
#' type priors, type posteriors, and other relevant elements.
#' See argument \code{object} for other
#'
#' @inheritParams CausalQueries_internal_inherit_params
#' @param object A character string specifying the component to retrieve.
#'   Available options are:
#'   \describe{
#'     \item{\code{"nodal_types"}}{}}
#' @return Elements from a \code{causal_model} as specified
#'
#' @export
#'
get <- function(model, object = "dag") {
  switch(object,
         dag = model$dag,
         statement = model$statement,
         nodes = model$nodes,
         parents_df = model$parents_df,
         parameters_df = model$parameters_df,
         causal_types = get_causal_types(model),
         nodal_types = get_nodal_types(model),
         parameter_matrix = get_parameter_matrix(model),
         parameter_prior = get_param_dist(model, using = "priors"),
         parameter_posterior = get_param_dist(model, using = "posteriors"),
         parameters = get_param_dist(model, using = "parameters"),
         prior_distribution = get_param_dist(model, using = "priors"),
         type_posterior =  get_type_prob_multiple(model, using = "posteriors") |> t() |> data.frame(),
         type_prior =  get_type_prob_multiple(model, using = "priors") |> t() |> data.frame(),
         event_probabilities =
           if (is.null(model$stan_objects$w)) {
           message("Event probabilities cannot be retrieved because keep_event_probabilities was set to FALSE")
         } else {
           model$stan_objects$w
         }
         )

}


