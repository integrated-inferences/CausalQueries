#' FUNCTION_TITLE
#'
#' FUNCTION_DESCRIPTION
#'
#' @param model DESCRIPTION.
#'
#' @return RETURN_DESCRIPTION
#' @examples
#' # ADD_EXAMPLES_HERE
#' @export
make_adjacency_matrix <- function(model) {
	dag <- model$dag
	vars <- gbiqq::get_variables(model)

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
#' @param model DESCRIPTION.
#'
#' @return RETURN_DESCRIPTION
#'
#' @export
#'
#' @examples
#' # ADD_EXAMPLES_HERE
expand_ambiguity_matrices_internal <- function(model) {

	max_possible_data <- get_max_possible_data(model)

	matrices_out <-
		mapply(possible_data = get_possible_data(model, collapse = FALSE)[get_endogenous_vars(model)],
					 ambiguity = make_ambiguity_matrices(model)[get_endogenous_vars(model)],
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
#' @param model DESCRIPTION.
#'
#' @return RETURN_DESCRIPTION
#'
#' @export
#'
#' @examples
#' # ADD_EXAMPLES_HERE
expand_ambiguity_matrices <- function(model) {
	dag <- model$dag
	ambiguity_ls <-
		lapply(expand_ambiguity_matrices_internal(model),
					 FUN = function(mat) {
					 	lapply(split(mat, f = seq(nrow(mat))), FUN = unlist)
					 })

	expand_grid_fn <- function(...) { Reduce(x = expand.grid(...), f = "*") }

	out <- t(do.call(mapply, append(list(FUN = expand_grid_fn), ambiguity_ls)))

	max_possible_data <- get_max_possible_data(model)

	rownames(out) <- apply(max_possible_data, 1, paste0, collapse = "")
	colnames(out) <-
		Reduce(x = lapply(gbiqq::get_types(model)[get_endogenous_vars(model)],
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
#' @param model A probabilistic causal model created by \code{make_model}
#'
#' @return A data frame that maps types (rows) to possible data realizations (columns).
#'
#' @export
#'
#' @examples
#'
#' XMYmodel <- make_model(add_edges(parent = "X", children = c("M")),
#'                      add_edges(parent = "M", children = c("Y")))
#'
#' get_ambiguity_matrix(model = XMYmodel)
#'
get_ambiguities_matrix <- function(model){
	dag <- model$dag
	# 1. Get nodal types. e.g. for X->Y: X0, X1, Y00, Y01..
	possible_data <-	get_possible_data(model)
	nodal_types   <-  gbiqq:::get_nodal_types(model)
	type_labels   <-  expand.grid(nodal_types)
	type_labels   <-  apply(type_labels, 1, paste0, collapse = "")

	# 2. Get types as the combination of possible data. e.g. for X->Y: X0Y00, X1Y00, X0Y10, X1Y10...
	types <- gbiqq:::get_expanded_types(model)

  # 3. Map types to data realizations. This is done in reveal_data
  data_realizations   <- gbiqq:::reveal_data(model)
  types$revealed_data <- apply(data_realizations , 1, paste0, collapse = "")

  # 4.  Create and return matrix A
  max_possible_data <- get_max_possible_data(model)
  data_names <- sapply(1:ncol(max_possible_data), function(i) paste0(colnames(max_possible_data)[i],max_possible_data[,i] ))
  data_names <- matrix(data_names, ncol = ncol(max_possible_data))
  data_names <- apply(data_names, 1, paste0, collapse = "")
  fundamental_data	<- apply(max_possible_data, 1, paste0, collapse = "")
  A <- sapply(1:nrow(types), function(i)(types$revealed_data[i] == fundamental_data)*1)
  A <- matrix(A, ncol = length(type_labels))
  colnames(A) <- type_labels
  rownames(A) <-  data_names

	return(A)
}

#' Reveal data
#'
#' Reveal data based on parents' data realizations and types of exogenous variables
#'
#' @details \code{reveal_data} starts off by creating types (via \code{\link{gbiq:::get_types}}). It then takes types of endogenous and reveals their outcome based on the value that their parents took. Exogenous variables outcomes correspond to their type.
#'
#' @param model A dag as created by \code{make_dag}
#' @return revealed_data
#' @keywords internal
#'
reveal_data <- function(model){

	types <- gbiqq::get_expanded_types(model)
	nodal_types <- get_nodal_types(model, collapse = FALSE)
	exogenous_vars <- get_exogenous_vars(model)
	endogenous_vars <- get_endogenous_vars(model)
	types_of_exogenous <-   data.frame(types[, exogenous_vars])
	names(types_of_exogenous) <- 	exogenous_vars
	types_of_endogenous <-  data.frame(types[, endogenous_vars], stringsAsFactors = FALSE)
	names(types_of_endogenous) <- 	endogenous_vars
	data_realizations <- 	types
	parents_list <- get_parents(model)

	revealed_data <- 		sapply(1:ncol(types_of_endogenous), function(j) {
		var <- names(types_of_endogenous)[j]
		child_type <- types_of_endogenous[,j]
		parents <- parents_list[[var]]
		nodal_type_var <- nodal_types[[var]]
		nodal_label <- apply(nodal_type_var, 1,paste, collapse ="")

		sapply(1:length(child_type), function(i){
			type <- child_type[i]
			parents_val <- data_realizations[i, parents]
			parents_val <- paste(parents_val, collapse = "")
			row <- which(nodal_label == type )
			outcome <- nodal_type_var[row, parents_val]
			outcome
		})})
	  data_realizations[, endogenous_vars] <- revealed_data
	  data_realizations
}



#' Get indicator matrix
#'
#' @param model A dag created by make_dag()
#'
#' @export
#'
get_indicator_matrix  <- function(model){


	possible_data <-	get_possible_data(model)
	nodes <- names(possible_data)
	nodal_types <- get_nodal_types(model)
	types <- expand.grid(nodal_types, stringsAsFactors = FALSE)
	row_identifiers <- unlist(nodal_types)

	# Which nodal_types correspond to a type
	indicator_matrix <- sapply(1:nrow(types), function(i){
		type <- types[i,]
		sapply(row_identifiers, function(nodal_type)
			all(nodal_type %in% type) )})*1


	colnames(indicator_matrix) <- do.call(paste, c(types, sep =""))
	rownames(indicator_matrix ) <- row_identifiers
	indicator_matrix
}


