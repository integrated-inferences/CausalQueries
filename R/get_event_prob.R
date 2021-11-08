#' Draw event probabilities
#'
# `get_event_prob` draws event probability vector `w` given a single realization of parameters
#'
#' @inheritParams CausalQueries_internal_inherit_params
#' @param type_prob A numeric vector. Type probabilities. (Not required).
#' @return An {array} of event probabilities
#' @export
#' @examples
#' \donttest{
#' model <- make_model('X -> Y')
#' get_event_prob(model = model)
#' get_event_prob(model = model, parameters = rep(1, 6))
#' get_event_prob(model = model, parameters = 1:6)
#' }
#'
# get_event_prob <- function(model, P = NULL, A = NULL, parameters = NULL, type_prob = NULL) {
#
#     # draw_event uses a parameter vector that is either provided directly or else drawn
#     # from priors or posteriors
#     # a directly provided parameter vector is used instead of parameters contained in the
#     # model
#
#     if (!is.null(parameters))
#         parameters <- clean_param_vector(model, parameters)
#     if (is.null(parameters))
#         parameters <- get_parameters(model)
#
#     A <- get_ambiguities_matrix(model)
#
#     # Type probabilities
#     if (is.null(type_prob))
#         type_prob <- get_type_prob(model = model, P = P, parameters = parameters)
#
#     # Event probabilities: this is for cases with only one possible data type
#     if (ncol(A) == 1) {
#         out <- matrix(1)
#         rownames(out) <- colnames(A)
#         return(out)
#     }
#
#     out <- t(A) %*% type_prob
#
#     colnames(out) <- "event_prob"
#     return(out)
# }

get_event_prob <- function(model, parameters = NULL, A = NULL, P = NULL){

    if(is.null(A)) A <- get_ambiguities_matrix(model)
    if(is.null(P)) P <- get_parameter_matrix(model)


    if (!is.null(parameters))
        parameters <- clean_param_vector(model, parameters)
    if (is.null(parameters))
        parameters <- get_parameters(model)

    parmap <- get_parmap(model, A = A, P = P)
    map <- t(attr(parmap, "map"))

    x <-
        (parmap * parameters) %>%
            data.frame() %>%
            group_by(model$parameters_df$node) %>%
            summarize_all(sum) %>%        # Probability of each node
     select(-1) %>%
     summarize_all(prod) %>%              # Probability of data type
     t

    # Reorder and export
    # rownames(x) == colnames(A)
    map %*% x
    # rownames(x) == colnames(A)

}

