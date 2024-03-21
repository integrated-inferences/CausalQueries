#' Get causal types
#'
#' Return data frame with types produced from all combinations of possible
#' data produced by a DAG.
#'
#' @inheritParams CausalQueries_internal_inherit_params
#' @keywords internal
#' @return A \code{data.frame} indicating causal types of a \code{model}
#' @examples
#' get_causal_types(make_model('X -> Y'))
#'

get_causal_types <- function(model) {

    if (!is.null(model$causal_types)) {
      return(model$causal_types)
    }

    return(update_causal_types(model))
}

