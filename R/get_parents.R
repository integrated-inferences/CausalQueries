

#' Get list of parents of all nodes in a model
#'
#' @inheritParams gbiqq_internal_inherit_params
#' @export
#' @return A \code{list} of parents in a DAG
#' @examples
#' model <- make_model('X -> K -> Y')
#' get_parents(model)

get_parents <- function(model) {
	dag <- model$dag
	sapply(model$nodes, function(j)
		paste(dag$parent)[paste(dag$children) == j])
}
