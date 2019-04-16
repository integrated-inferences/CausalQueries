#' FUNCTION_TITLE
#'
#' FUNCTION_DESCRIPTION
#'
#' @param pcm DESCRIPTION.
#'
#' @return RETURN_DESCRIPTION
#' @examples
#' # ADD_EXAMPLES_HERE
#' @export
make_adjacency_matrix <- function(pcm) {
	dag <- pcm$dag
	vars <- gbiqq::get_variables(pcm)

	adj_matrix <-
		matrix(FALSE,
					 nrow = length(vars),
					 ncol = length(vars))

	colnames(adj_matrix) <- rownames(adj_matrix) <- vars

	for (i in 1:nrow(dag)) {
		adj_matrix[as.character(dag$children[i]), as.character(dag$parent[i])] <- TRUE
	}

	return(adj_matrix)

}

#' FUNCTION_TITLE
#'
#' FUNCTION_DESCRIPTION
#'
#' @param pcm DESCRIPTION.
#'
#' @return RETURN_DESCRIPTION
#'
#' @export
#'
#' @examples
#' # ADD_EXAMPLES_HERE
expand_ambiguity_matrices_internal <- function(pcm) {

	max_possible_data <- get_max_possible_data(pcm)

	matrices_out <-
		mapply(possible_data = get_possible_data(pcm, collapse = FALSE)[get_endogenous_vars(dag)],
					 ambiguity = make_ambiguity_matrices(pcm)[get_endogenous_vars(dag)],
					 FUN = function(possible_data, ambiguity) {
					 	out <- merge(max_possible_data, cbind(possible_data,ambiguity), all.x = TRUE)
					 	out <- out[do.call("order", as.list(out[,rev(names(max_possible_data))])),]
					 	if (!all(max_possible_data == out[,names(max_possible_data)]))
					 		stop("The naming of rows is an issue in expand_ambiguity_matrices_internal.")
					 	rownames(out) <- apply(out[,names(max_possible_data)], 1, paste0, collapse = "")
					 	out <- out[,!(names(out) %in% names(max_possible_data))]
					 	return(out)
					 },
					 SIMPLIFY = FALSE)

	return(matrices_out)

}


#' FUNCTION_TITLE
#'
#' FUNCTION_DESCRIPTION
#'
#' @param pcm DESCRIPTION.
#'
#' @return RETURN_DESCRIPTION
#'
#' @export
#'
#' @examples
#' # ADD_EXAMPLES_HERE
expand_ambiguity_matrices <- function(dag) {
	dag <- pcm$dag
	ambiguity_ls <-
		lapply(expand_ambiguity_matrices_internal(dag),
					 FUN = function(mat) {
					 	lapply(split(mat, f = seq(nrow(mat))), FUN = unlist)
					 })

	expand_grid_fn <- function(...) { Reduce(x = expand.grid(...), f = "*") }

	out <- t(do.call(mapply, append(list(FUN = expand_grid_fn), ambiguity_ls)))

	max_possible_data <- get_max_possible_data(dag)

	rownames(out) <- apply(max_possible_data, 1, paste0, collapse = "")
	colnames(out) <-
		Reduce(x = lapply(gbiqq::get_types(dag)[get_endogenous_vars(dag)],
											FUN = function(types) as.character(1:nrow(types))),
					 f = function(a,b) {
					 	apply(expand.grid(x = a, y = b), 1, paste0, collapse = "_")
					 })

	return(out)

}

# New functions ------------------------------------------------------------------

#' Get ambiguities matrix
#'
#' Get matrix that maps types to data realizations
#'
#' @param dag A dag as created by \code{make_dag}
#'
#' @return A data frame that maps types (columns) to possible data realizations (rows).
#'
#' @export
#'
#' @examples
#'
#' XMYdag <- make_dag(add_edges(parent = "X", children = c("M")),
#'                    add_edges(parent = "M", children = c("Y")))
#'
#' get_ambiguity_matrix(dag = XMYdag)
#'
get_ambiguities_matrix <- function(pcm){
	dag <- pcm$dag
	# 1. Get nodal types. e.g. for X->Y: X0, X1, Y00, Y01..
	possible_data <-	get_possible_data(pcm)
	nodal_types <-   gbiqq:::get_nodal_types(pcm)
	type_labels <- expand.grid(nodal_types)
	type_labels <- apply(type_labels, 1, paste0, collapse = "")

	# 2. Get types as the combination of possible data. e.g. for X->Y: X0Y00, X1Y00, X0Y10, X1Y10...
	types <- gbiqq:::get_expanded_types(pcm)

  # 3. Map types to data realizations. This is done in reveal_data
  data_realizations <- gbiqq:::reveal_data(pcm)
  types$revealed_data <- apply(data_realizations , 1, paste0, collapse = "")

  # 4.  Create and return matrix A
  max_possible_data <- get_max_possible_data(pcm)
  fundamental_data <- sapply(1:ncol(max_possible_data), function(i) paste0(colnames(max_possible_data)[i],max_possible_data[,i] ))
  fundamental_data	<- apply(fundamental_data, 1, paste0, collapse = "")
  A <- sapply(1:nrow(types), function(i)(types$revealed_data[i] == fundamental_data)*1)
  colnames(A) <- type_labels
  rownames(A) <- fundamental_data

	return(A)
}

#' Reveal data
#'
#' Reveal data based on parents' data realizations and types of exogenous variables
#'
#' @details \code{reveal_data} starts off by creating types (via \code{\link{gbiq:::get_types}}). It then takes types of endogenous and reveals their outcome based on the value that their parents took. Exogenous variables outcomes correspond to their type.
#'
#' @param pcm A dag as created by \code{make_dag}
#' @return revealed_data
#' @keywords internal
#'
reveal_data <- function(pcm){

	types <- gbiqq:::get_expanded_types(pcm)
	exogenous_vars <- get_exogenous_vars(pcm)
	endogenous_vars <- get_endogenous_vars(pcm)
	types_of_exogenous <-   data.frame(types[, exogenous_vars])
	names(types_of_exogenous) <- 	exogenous_vars
	types_of_endogenous <-  data.frame(types[, endogenous_vars], stringsAsFactors = FALSE)
	names(types_of_endogenous) <- 	endogenous_vars
	data_realizations <- 	types
	parents_list <- get_parents(dag)

	revealed_data <- 		sapply(1:ncol(types_of_endogenous), function(j) {
		var <- names(types_of_endogenous)[j]
		child_type <- types_of_endogenous[,j]
		parents <- parents_list[[var]]

		sapply(1:length(child_type), function(i){
			type <- child_type[i]
			type_ <- unlist(strsplit(type, split = ""))
			outcome_index <- nchar(type)
			outcome <- type_[outcome_index]
			predecessors <- type_[1:(outcome_index-1)]
			affects <- predecessors != outcome
			causes <- parents[affects]
			value_of_causes <- as.numeric(data_realizations[i, causes])
			revealed_outcome <- ifelse(all(!affects), outcome, prod(value_of_causes == outcome))
		})})
	  data_realizations[, endogenous_vars] <- revealed_data
	  data_realizations
}

#' Get expanded types
#'
#' Create a data frame with types produced from all combinations of possible data produce by a dag.
#'
#' @param pcm A dag as created by \code{make_dag}
#'
#' @return types
#' @keywords internal
#'
get_expanded_types <- function(pcm){

	possible_data <-	get_possible_data_internal(pcm)
 # Get types as the combination of nodal types/possible_data. for X->Y: X0Y00, X1Y00, X0Y10, X1Y10...
	expand.grid(possible_data, stringsAsFactors = FALSE)
}

#' Get nodal types
#' Nodal types are created by concatening variables and their possible data. Used for labeling ambiguities matrix.
#'
#' @param pcm A dag as created by \code{make_dag}
#'
#' @return types
#' @keywords internal
#'
get_nodal_types <- function(pcm){
if(!is.null(pcm$nodal_types )){
	return(pcm$nodal_types )
} else{



possible_data <-	get_possible_data(pcm)
nodes <- names(possible_data)

return(mapply(function(realization, node) paste0(node, realization),
											 node = nodes,
											 realization = possible_data ))
}
}
