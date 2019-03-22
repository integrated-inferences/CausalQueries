
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
		var_variables <- c(var_parents,variable)
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
	# types <- get_types(dag)
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

#' Get helpers for expanding the pi matrix
#'
#' @param dag A dag created by make_dag()
#'
#' @export
#'
#' @return A list of pi helpers
get_pi_expanders <- function(pi,dag){
	types <- get_n_endogenous_types(dag)
	lapply(pi,
				 FUN = function(exogenous_var) {

				 	pos <- which(names(exogenous_var) == names(types))
				 	select_vars <-
				 		cumsum((seq_along(names(types)) == min(pos)) +
				 					 	(seq_along(names(types)) == (max(pos) + 1)))

				 	return(list(times = prod(types[select_vars == 2]),
				 							each = prod(types[select_vars == 0])))

				 })
}







#' Get the multinomial data events
#'
#' @param data A data.frame of variables that can take three values: 0, 1, and NA
#' @param dag A dag created by make_dag()
#'
#' @export
#'
#' @return A vector of data events
get_data_events <- function(data, dag){
	possible_events <- get_likelihood_helpers(dag)$possible_events
	possible_strategies <- unique(names(possible_events))
	variables <- get_variables(dag)

	if(!all(variables %in% names(data))){
		stop("Could not find all of the variables in the DAG in the data you provided.\nPlease double-check variable names and try again.")
	}

	data <- data[,variables]

	data_to_agg <- data
	data_to_agg[is.na(data_to_agg)] <- ""

	observed_events <- apply(
		X = data_to_agg,
		MARGIN = 1,
		FUN = function(row){
			variable_labels <- variables[!row == ""]
			row_subset <- row[!row == ""]
			paste(variable_labels,row_subset,sep = "")
		})
	used_strategies <- unique(apply(
		X = data_to_agg,
		MARGIN = 1,
		FUN = function(row){
			strategy <- paste(variables[!row == ""],collapse = "")
			return(strategy)
		}))

	observed_events <- sapply(observed_events,paste,collapse = "")
	observed_events <- table(observed_events)

	unobserved_event_labels <- possible_events[!possible_events %in% names(observed_events)]

	unused_strategies <- possible_strategies[!possible_strategies %in% used_strategies]

	unobserved_events <- rep(0,length(unobserved_event_labels))

	names(unobserved_events) <- unobserved_event_labels

	data_events <- data.frame(
		event = names(c(observed_events,unobserved_events)),
		count = c(observed_events,unobserved_events),
		row.names = NULL
	)
	data_events <- data_events[match(possible_events,data_events$event),]

	return(list(
		data_events = data_events,
		observed_events = observed_events,
		unobserved_events = unobserved_events,
		used_strategies = used_strategies,
		unused_strategies = unused_strategies
	))

}





#' Get indicator matrix
#'
#' @param dag A dag created by make_dag()
#'
#' @export
#'
get_indicator_matrix  <- function(dag){

	possible_data <-	get_possible_data(dag)
	nodes <- names(possible_data)
	nodal_types <-  mapply(function(realization, node) paste0(node, realization),
												 node = nodes,
												 realization = possible_data )

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









