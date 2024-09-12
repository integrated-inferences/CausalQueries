#' Get type probabilities
#'
#' Gets probability of vector of causal types given a single
#' realization of parameters, possibly drawn from model priors.
#'
#' By default, parameters is drawn from `using` argument
#' (either from priors, posteriors, or from inspect(model, "parameters"))
#'
#'@inheritParams CausalQueries_internal_inherit_params
#' @return A vector with probabilities of vector of causal types
#' @export

get_type_prob <- function(model,
                          P = NULL,
                          parameters = NULL) {

    if(!is.null(parameters)) {
      parameters <- clean_param_vector(model, parameters)
    }

    if(is.null(parameters)) {
      parameters <- get_parameters(model)
    }

    if (is.null(P)) {
      P <- get_parameter_matrix(model)
    }

    # Type probabilities
    get_type_prob_c(P = as.matrix(P), parameters = parameters)
}

#' Draw matrix of type probabilities, before or after estimation
#'
#' @inheritParams CausalQueries_internal_inherit_params
#'
#' @param using A character. It indicates whether to use
#'   `priors`, `posteriors` or `parameters`.
#' @param n_draws An integer. If no prior distribution is provided, generate
#'   prior distribution with \code{n_draws} number of draws.
#' @param param_dist A \code{matrix}.  Distribution of parameters.
#'   Optional for speed.
#' @keywords internal
#' @return A \code{matrix} of type probabilities.

get_type_prob_multiple <- function(model,
                                   using = "priors",
                                   parameters = NULL,
                                   n_draws = 4000,
                                   param_dist = NULL,
                                   P = NULL) {
    # Posteriors are pulled  from stan object
    if(!is.null(model$stan_objects$type_distribution) & using == "posteriors") {
      return(t(model$stan_objects$type_distribution))
    }

    if(is.null(P)) {
      P <- get_parameter_matrix(model)
    }

    if(using == "parameters") {
        if(is.null(parameters)) {
          parameters <- get_parameters(model)
        }
        return(get_type_prob(model, parameters = parameters, P = P))
    }

    # Do one at a time
    if(is.null(param_dist)) {
      param_dist <- get_param_dist(model, using, n_draws = n_draws) |>
        as.matrix()
    }

    res <- get_type_prob_multiple_c(params = param_dist, P = as.matrix(P))
    rownames(res) <- colnames(P)
    class(res) <- c("type_prior", "matrix", "array")
    return(res)
}


#' Get a distribution of model parameters
#'
#' Using parameters, priors, or posteriors
#'
#' @inheritParams CausalQueries_internal_inherit_params
#' @keywords internal
#' @return A \code{matrix} with the distribution of the parameters in the model

get_param_dist <- function(model,
                           using,
                           n_draws = 4000) {

    if(using == "parameters") {
      return(get_parameters(model))
    }

    if(using == "priors") {
        if(is.null(model$prior_distribution)) {
            message("Prior distribution added to model")
            model <- set_prior_distribution(model, n_draws = n_draws)
        }
        return(model$prior_distribution)
    }

    if(using == "posteriors") {
        if(is.null(model$posterior_distribution)) {
            stop("Model does not contain a posterior distribution")
        }
        return(model$posterior_distribution)
    }
}
