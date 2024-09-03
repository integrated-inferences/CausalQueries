#' Draw event probabilities
#'
#' `get_event_probabilities` draws event probability vector `w` given a single
#' realization of parameters
#'
#' @inheritParams CausalQueries_internal_inherit_params
#' @param given A string specifying known values on nodes, e.g. "X==1 & Y==1"
#' @return An array of event probabilities
#' @export
#' @examples
#' \donttest{
#' model <- make_model('X -> Y')
#' get_event_probabilities(model = model)
#' get_event_probabilities(model = model, given = "X==1")
#' get_event_probabilities(model = model, parameters = rep(1, 6))
#' get_event_probabilities(model = model, parameters = 1:6)
#' }
#'

get_event_probabilities <- function(model,
                           parameters = NULL,
                           A = NULL,
                           P = NULL,
                           given = NULL){

    if (!is.null(parameters)) {
      parameters <- clean_param_vector(model, parameters)
    }

    if (is.null(parameters)) {
      parameters <- get_parameters(model)
    }

    parmap <- get_parmap(model, A = A, P = P)
    map <- t(attr(parmap, "map"))

    x <-
        (parmap * parameters) %>%
            data.frame() %>%
            group_by(model$parameters_df$node) %>%
            summarize_all(sum) %>%        # Probability of each node
     select(-1) %>%
     summarize_all(prod) %>%              # Probability of data type
     t()

    # Reorder s.t. rownames(x) == colnames(A)
    event_probs <- map %*% x

    # Produce conditional probabilities in the event that data is given
    if(!is.null(given)) {
      event_probs <- model %>%
        get_all_data_types(complete_data = TRUE) %>%
        mutate(
          event_probs = event_probs,
          event_probs = ifelse(eval(parse(text = given)), event_probs, 0),
          event_probs = event_probs/sum(event_probs)) %>%
        select(event_probs) %>% as.matrix()
    }

    colnames(event_probs) <- "event_probs"
    class(event_probs) <- c("event_probabilities", "matrix", "array")
    return(event_probs)
}

