#' Get causal types
#'
#' Create a data frame with types produced from all combinations of possible data produced by a DAG.
#'
#' @inheritParams gbiqq_internal_inherit_params
#' @return A code{data.frame} indicating causal types of \code{model}
#' @export
#'
get_causal_types <- function(model) {

    if (!is.null(model$causal_types))
        return(model$causal_types)

    update_causal_types(model)
}

