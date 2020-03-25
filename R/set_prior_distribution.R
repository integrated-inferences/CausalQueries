#' Make a prior distribution from priors
#'
#' Add n_param x n_draws database of possible lambda draws to the model.
#'
#' @inheritParams gbiqq_internal_inherit_params
#' @param n_draws A scalar. Number of draws.
#' @export
#' @importFrom gtools rdirichlet
#' @family prior_distribution
#' @examples
#' make_model('X -> Y') %>% make_prior_distribution(n_draws = 5)
#'
make_prior_distribution <- function(model, n_draws = 4000) {

    param_sets <- unique(model$parameters_df$param_set)

    priors <- model$parameters_df$priors

    parameters <- unlist(sapply(param_sets, function(v) rdirichlet(n_draws, priors[model$parameters_df$param_set ==
        v])))

    prior_distribution <- matrix(parameters, nrow = n_draws)

    colnames(prior_distribution) <- model$parameters_df$param_names

    prior_distribution

}


#' Get a prior distribution from priors
#'
#' Add to the model a n_draws x n_param matrix of possible parameters.
#'
#' @inheritParams gbiqq_internal_inherit_params
#' @param n_draws A scalar. Number of draws.
#' @export
#' @family prior_distribution
#' @examples
#' make_model('X -> Y') %>% set_prior_distribution(n_draws = 5) %>% get_prior_distribution()
#' make_model('X -> Y') %>% get_prior_distribution(3)
#'
get_prior_distribution <- function(model, n_draws = 4000) {

    if (!is.null(model$prior_distribution))
        return(model$prior_distribution)

    message("The model does not have an attached prior distribution; generated on the fly")

    make_prior_distribution(model, n_draws)
}

#' Add prior distribution draws
#'
#' Add n_param x n_draws database of possible lambda draws to the model.
#'
#' @inheritParams gbiqq_internal_inherit_params
#' @param n_draws A scalar. Number of draws.
#' @export
#' @family prior_distribution
#' @examples
#' make_model('X -> Y') %>% set_prior_distribution(n_draws = 5) %>% get_prior_distribution()
#'
set_prior_distribution <- function(model, n_draws = 4000) {

    model$prior_distribution <- make_prior_distribution(model, n_draws = n_draws)

    model

}

