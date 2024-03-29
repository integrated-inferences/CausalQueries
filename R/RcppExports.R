# Generated by using Rcpp::compileAttributes() -> do not edit by hand
# Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#' generates one draw from type probability distribution for each type in P
#'
#' @param P parameter_matrix of parameters and causal types
#' @param parameters, priors or posteriors
#' @return draw from type distribution for each type in P
get_type_prob_c <- function(P, parameters) {
    .Call(`_CausalQueries_get_type_prob_c`, P, parameters)
}

#' generates n draws from type probability distribution for each type in P
#'
#' @param params parameters, priors or posteriors
#' @param P parameter_matrix of parameters and causal types
#' @return draws from type distribution for each type in P
get_type_prob_multiple_c <- function(params, P) {
    .Call(`_CausalQueries_get_type_prob_multiple_c`, params, P)
}

realise_outcomes_c <- function(real, parents_list, endogenous_vars, n_types) {
    .Call(`_CausalQueries_realise_outcomes_c`, real, parents_list, endogenous_vars, n_types)
}

