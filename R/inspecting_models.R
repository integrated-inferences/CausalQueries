
#' Get list of parents in a dag
#'
#' @param A probabilistic causal model created by make_model()
#'
#' @export
#' @return A list of parents in a DAG
#'
#' @examples
#'
#' \dontrun{
#' dag <- make_dag(
#'  add_edges(parent = "X",children = c("K","Y")),
#'  add_edges(parent = "K",children = "Y")
#' )
#'
#' get_parents(dag)
#' }
#'
#' dag <- make_dag(
#'  add_edges(parent = "X",children = c("K")),
#'  add_edges(parent = "K",children = "Y")
#' )
#'
#' get_parents(dag)
#' get_ancestors(dag)


get_parents <- function(model) {
	dag <- model$dag
	sapply(paste(unique(unlist(dag))), function(j) paste(dag$parent)[paste(dag$children) == j])
}


#' Get expanded types
#'
#' Create a data frame with types produced from all combinations of possible data produce by a dag.
#'
#' @param model A model as created by \code{make_model}
#'
#' @return types
#' @export
#'
get_causal_types <- function(model){
	if(!is.null(model$causal_types)){
		return_df <- model$causal_types
	} else {
		return_df <- update_causal_types(model)
	}
	return(return_df)
}






