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

    # Factorized stamp: parameters-only event probs (VE-backed helpers live in
    # event_prob_ve.R; full conditional still uses the complete grid under N*).
    if (!isTRUE(resolve_legacy(NULL, model))) {
      return(event_prob_factorized(model, parameters = parameters, given = given))
    }

    parmap <- get_parmap(model, A = A, P = P)
    map <- t(attr(parmap, "map"))

    x <- rowsum(parmap * parameters,
                group = model$parameters_df$node,
                reorder = FALSE)
    x <- apply(x, 2, prod)

    # Reorder s.t. rownames(x) == colnames(A)
    event_probs <- map %*% x

    # Condition on observed data. Align `given` with possible events
    # (rows of event_probs), not the full 2^n complete-data grid.
    if (!is.null(given)) {
      types <- get_all_data_types(model, complete_data = TRUE)
      i <- match(rownames(event_probs), rownames(types))
      if (anyNA(i)) {
        stop("Event names do not match complete data types.")
      }
      matches <- with(types[i, , drop = FALSE], eval(parse(text = given)))
      matches[is.na(matches)] <- FALSE
      w <- as.numeric(event_probs)
      w[!matches] <- 0
      s <- sum(w)
      if (!(s > 0)) {
        stop("No probability mass matches `given`.")
      }
      event_probs[, 1] <- w / s
    }

    colnames(event_probs) <- "event_probs"
    class(event_probs) <- c("matrix", "array")
    return(event_probs)
}

