

#' Get list of parents of all nodes in a model
#'
#' @inheritParams CausalQueries_internal_inherit_params
#' @export
#' @return A \code{list} of parents in a DAG
#' @examples
#' model <- make_model('X -> K -> Y')
#' get_parents(model)

get_parents <- function(model) {
  sapply(model$nodes, function(j) {
    paste(model$dag$parent)[paste(model$dag$children) == j]
  })
}

