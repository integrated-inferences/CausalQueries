
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
	}
	else{
		possible_types <-	get_nodal_types(model)
		variables <- names(possible_types)
		possible_types <- lapply(variables, function(v) gsub(v, "", possible_types[[v]]))
		names(possible_types) <- variables
		# Get types as the combination of nodal types/possible_data. for X->Y: X0Y00, X1Y00, X0Y10, X1Y10...
		return_df <- expand.grid(possible_types, stringsAsFactors = FALSE)
	}
	return(return_df)
}






