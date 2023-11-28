#' Make a prior distribution from priors
#'
#' Create a `n_param`x `n_draws` database of possible lambda draws to be
#' attached to the model.
#'
#' @inheritParams CausalQueries_internal_inherit_params
#' @param n_draws A scalar. Number of draws.
#' @return A `data.frame` with dimension `n_param`x `n_draws` of possible
#'   lambda draws
#' @export
#' @importFrom dirmult rdirichlet
#' @family prior_distribution
#' @examples
#' make_model('X -> Y') %>% make_prior_distribution(n_draws = 5)
#'
make_prior_distribution <- function(model, n_draws = 4000) {

    param_sets <- unique(model$parameters_df$param_set)

    priors <- model$parameters_df$priors

    prior_distribution <-
      lapply(param_sets, function(v) {
        dirmult::rdirichlet(n_draws, priors[model$parameters_df$param_set == v])
      })

    prior_distribution <- do.call("cbind", prior_distribution) |>
      as.data.frame()

    colnames(prior_distribution) <- model$parameters_df$param_names

    return(prior_distribution)

}


#' Get a prior distribution from priors
#'
#' Add to the model a `n_draws x n_param` matrix of possible parameters.
#'
#' @inheritParams CausalQueries_internal_inherit_params
#' @param n_draws A scalar. Number of draws.
#' @return A `data.frame` with dimension `n_param`x `n_draws` of possible
#'   lambda draws
#' @export
#' @family prior_distribution
#' @examples
#' make_model('X -> Y') %>%
#'   set_prior_distribution(n_draws = 5) %>%
#'   get_prior_distribution()
#' make_model('X -> Y') %>%
#'   get_prior_distribution(3)
#'

get_prior_distribution <- function(model, n_draws = 4000) {
  if (!is.null(model$prior_distribution)) {
    return(model$prior_distribution)
  }

  message(paste(
    "The model does not have an attached prior distribution;",
    "generated on the fly"
  ))

  return(make_prior_distribution(model, n_draws))
}

#' Add prior distribution draws
#'
#' Add `n_param x n_draws` database of possible lambda draws to the model.
#'
#' @inheritParams CausalQueries_internal_inherit_params
#' @param n_draws A scalar. Number of draws.
#' @return An object of class \code{causal_model}. It essentially returns a
#'   list containing the elements comprising a model
#'   (e.g. 'statement', 'nodal_types' and 'DAG') with the `prior_distribution`
#'   attached to it.
#' @export
#' @family prior_distribution
#' @examples
#' make_model('X -> Y') %>%
#'   set_prior_distribution(n_draws = 5) %>%
#'   get_prior_distribution()
#'

set_prior_distribution <- function(model, n_draws = 4000) {
  model$prior_distribution <-
    make_prior_distribution(model, n_draws = n_draws)

  return(model)
}

