#' Wrapper of types_to_nodes_single
#' Sapply multiple query conditions
#'
#' @param model A model created by make_model()
#' @param query a query
#' @keywords internal
types_to_nodes <- function(model, query){

	if (length(query) == 1){
		return_nodal_types <- types_to_nodes_single(model, query)

	} else 	if (length(query) > 1){

		return_nodal_types <- sapply(query, function(q) gbiqq:::types_to_nodes_single(model, q), USE.NAMES = FALSE)
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
	restricted_causal_types <- get_types(model, query = query)
	causal_types <- causal_types[!restricted_causal_types$types,]
	rownames(causal_types) <- 1:nrow(causal_types)

	type_names  <- sapply(1:ncol(causal_types), function(j) paste0(names(causal_types)[j], causal_types[,j]))
	unrestricted_nodal_types <- lapply(as.data.frame(type_names, stringsAsFactors = FALSE), unique)
	names(unrestricted_nodal_types) <- model$variables
	current_nodal_types <- get_nodal_types(model)

	return_nodal_types <- sapply(model$variables, function(v) setdiff(current_nodal_types[[v]], 	unrestricted_nodal_types[[v]] ))

	return_nodal_types <- return_nodal_types[lengths(	return_nodal_types) > 0L]


	return_nodal_types

}





