#' Wrapper of types_to_nodes_single
#' Sapply multiple query conditions
#'
#' @param model A model created by make_model()
#' @param query a query
#' @export
types_to_nodes <- function(model, query){

	if (length(query) == 1){
		return_nodal_types <- types_to_nodes_single(model, query)

	} else 	if (length(query) > 1){

		return_nodal_types <- sapply(query, function(q) types_to_nodes_single(model, q), USE.NAMES = FALSE)
		vars <- 	model$variables[model$variables %in% names(return_nodal_types)]
		return_nodal_types <- sapply(vars, function(v){
			i <- which(names(return_nodal_types) == v)
			out <- return_nodal_types[i]
			names(out) <- query[i]
			out}, simplify = FALSE)


	}


	return(return_nodal_types)
}





#' Identify nodes that satisfy a causal type query
#'
#' @param model A model created by make_model()
#' @param query a query
#' @keywords internal
#' @importFrom  rlang is_empty
types_to_nodes_single <- function(model, query){


	causal_types <- get_causal_types(model)
	mapped_causal_types <- get_types(model, query = query)
	# get causal types that don't map to query
	unlinked_causal_types <- causal_types[!mapped_causal_types$types,]
	rownames(unlinked_causal_types) <- 1:nrow(unlinked_causal_types)

	# unlinked_nodal_types: the nodal types that when combined produce the unliked causal types
	type_names  <- sapply(1:ncol(causal_types), function(j) paste0(names(unlinked_causal_types)[j], unlinked_causal_types[,j]))
  unlinked_nodal_types <- lapply(as.data.frame(type_names, stringsAsFactors = FALSE), unique)
	variables <- names(unlinked_nodal_types) <- names(causal_types)

	model_nodal_types <- get_nodal_types(model)

	# get those nodal types that are in the model but not in the unlinked
	return_nodal_types <- sapply(variables, function(v) setdiff(model_nodal_types[[v]], 	unlinked_nodal_types[[v]] ))


	return_nodal_types <- return_nodal_types[lengths(	return_nodal_types) > 0L]


	return_nodal_types

}





