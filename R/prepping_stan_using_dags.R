
#' This creates a list of all possible data realizations, for each child
#' @param dag A dag created by \code{make_dag()}
#' @param collapse whaether to collapse possible data
#'
#' @export
#'
#' @return A list of all data realizations possible
get_possible_data <- function(dag,
															collapse = TRUE){
	variables <- get_variables(dag)
	parents <- get_parents(dag)
	# types <- get_types(dag)

	possible_data_list <- list()
	for (variable in variables) {
		# var_types <- types[variable][[1]]
		var_parents <- parents[variable][[1]]
		var_variables <- c(variable,var_parents)
		possible_data <- do.call(
			what = "expand.grid",
			args = lapply(
				X = var_variables,
				FUN = function(x) 0:1
			)
		)
		if (collapse) {
			possible_data <- apply(possible_data,1,paste,collapse = "")
		} else if (!collapse) {
			names(possible_data) <- var_variables
		}

		possible_data_list[variable][[1]] <- possible_data

	}
	return(possible_data_list)
}

#' Higher level function that returns all of the type ambiguities for all possible patterns of evidence
#'
#' @param dag A dag created by make_dag()
#'
#' @export
#'
#' @return A list of mappings between types
get_ambiguities <- function(dag){
	variables <- get_variables(dag)
	# parents <- get_parents(dag)
	types <- get_types(dag)

	possible_data <- get_possible_data(dag)

	ambiguities <- list()

	for (variable in variables) {
		ambiguities[variable][[1]] <- get_ambiguities_internal(
			sub_types = types[variable][[1]],
			possible_data = possible_data[variable][[1]]
		)
	}

	return(ambiguities)

}

get_ambiguities_internal <-
	function(sub_types, possible_data) {
		parent_values <- colnames(sub_types)
		realizations <- sub_types

		for (parval in parent_values) {
			realizations[,parval] <- paste0(parval,realizations[,parval])
		}

		type_ambiguities <- t(
			sapply(
				X = possible_data,
				FUN = function(x) {
					which(
						apply(
							X = realizations,
							MARGIN = 1,
							FUN = function(y) x %in% y
						)
					)
				}
			)
		)

		# If there are no parents, must coerce back to matrix
		if (all(dim(type_ambiguities) == c(1,2))) {
			type_ambiguities <- t(as.matrix(type_ambiguities))
		}
		return(type_ambiguities)
	}


#' Internal function that transforms the ambiguity list into an indicator
#' function
#' @param ambiguities Ambiguities matrix produced by \code{get_ambiguities()}
make_ambiguity_matrix_internal <- function(ambiguities){
	n_types <- max(ambiguities)

	amb_mat <- matrix(data = 0,nrow = nrow(ambiguities),ncol = n_types)

	for (i in 1:nrow(ambiguities)) {
		amb_mat[i,ambiguities[i,]] <- 1
	}

	return(amb_mat)

}

#' Higher level function that returns an indicator matrix showing correspondence between type and possible evidence pattern for each type and evidence pattern
#'
#' @param dag A dag created by make_dag()
#'
#' @export
#'
#' @return A list of mappings between types
make_ambiguity_matrices <- function(dag){
	ambiguities <- get_ambiguities(dag)
	possible_data <- get_possible_data(dag)

	ambiguity_matrices <- list()
	for (variable in names(ambiguities)) {
		ambiguity_matrices[variable][[1]] <- make_ambiguity_matrix_internal(
			ambiguities = ambiguities[variable][[1]]
		)

		rownames(ambiguity_matrices[variable][[1]]) <- possible_data[variable][[1]]
		colnames(ambiguity_matrices[variable][[1]]) <- 1:ncol(ambiguity_matrices[variable][[1]])

	}
	return(ambiguity_matrices)
}


#' Map types of every varibale to observable data
#'
#' @param dag A dag created by make_dag()
#'
#' @export
#'
#' @return A list of mappings between types and data
map_types_to_data <- function(dag){
	possible_data <- get_possible_data(dag)
	types <- get_types(dag)
	variables <- get_variables(dag)
	# type_indices <- get_type_indices(dag)
	parents <- get_parents(dag)
	n_vars <- sapply(parents,length) + 1
	ambiguities <- get_ambiguities(dag)

	# Get the data at the terminal node
	full_data <- possible_data[variables[length(variables)]][[1]]

	type_mapping <- list()
	for (variable in variables) {
		row_indices <- substr(full_data,start = 1,stop = n_vars[variable])
		type_map <- ambiguities[variable][[1]][row_indices,]
		if (is.null(dim(type_map))) dim(type_map) <- c(length(type_map),1)
		n_types <- max(type_map)
		amb_mat <- matrix(data = 0,nrow = nrow(type_map),ncol = n_types)
		for (i in 1:nrow(type_map)) {
			amb_mat[i,type_map[i,]] <- 1
		}
		rownames(amb_mat) <- full_data
		colnames(amb_mat) <- 1:n_types
		type_mapping[[variable]] <- amb_mat
	}

	return(type_mapping)

}

