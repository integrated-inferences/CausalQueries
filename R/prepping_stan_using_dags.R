
#' This creates a list of all possible data realizations, for each child
#' @param pcm A dag created by \code{make_dag()}
#' @param collapse whaether to collapse possible data
#'
#' @export
#'
#' @return A list of all data realizations possible
get_possible_data <- function(pcm,
															collapse = TRUE){

	variables <- get_variables(pcm)
	parents <- get_parents(pcm)
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
		names(possible_data) <- var_variables



		if (collapse) {
			possible_data <- apply(possible_data,1,paste,collapse = "")
		} else {
			names(possible_data) <- var_variables
		}

		possible_data_list[variable][[1]] <- possible_data

	}


	names(possible_data_list) <- variables
	return(possible_data_list)
}

#' Higher level function that returns all of the type ambiguities for all possible patterns of evidence
#'
#' @param pcm A dag created by make_dag()
#'
#' @export
#'
#' @return A list of mappings between types
get_ambiguities <- function(pcm){

	variables <- get_variables(pcm)
	# parents <- get_parents(pcm)
	types <- get_types(pcm)

	possible_data <- get_possible_data(pcm)

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
make_ambiguity_matrices <- function(pcm){

	ambiguities <- get_ambiguities(pcm)
	possible_data <- get_possible_data(pcm)

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
#' @param pcm A dag created by make_dag()
#'
#' @export
#'
#' @return A list of mappings between types and data
map_types_to_data <- function(pcm){

	possible_data <- get_possible_data(pcm)
	# types <- get_types(dag)
	variables <- get_variables(pcm)
	# type_indices <- get_type_indices(dag)
	parents <- get_parents(pcm)
	n_vars <- sapply(parents,length) + 1
	ambiguities <- get_ambiguities(pcm)

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
#' @param pcm A dag created by make_dag()
#'
#' @export
#'
#' @return A list of pi helpers
get_pi_expanders <- function(pi,pcm){

	types <- get_n_endogenous_types(pcm)
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
#' @param pcm A dag created by make_dag()
#'
#' @export
#'
#' @return A vector of data events
get_data_events <- function(data, pcm){

	likelihood_helpers <- get_likelihood_helpers(pcm)
	possible_events <- likelihood_helpers$possible_events
	possible_strategies <- names(likelihood_helpers$w_starts)
	variables <- get_variables(pcm)
	i_strategy <- likelihood_helpers$w_ends - likelihood_helpers$w_starts +1


	if(!all(variables %in% names(data))){
		stop("Could not find all of the variables in the DAG in the data you provided.\nPlease double-check variable names and try again.")
	}

	# replace "" with na and remove rows where all values are na
	data <- data[,variables]
	data_to_agg <- data
	data_to_agg[data == ""] <- NA
  ind <- apply(data_to_agg, 1,  function(x) all(is.na(x)))
  data_to_agg <- data_to_agg[!ind, ]

	observed_events <- apply(data_to_agg , 1, function(row){
	observed_variables <- variables[!is.na(row)]
	row <- row[!is.na(row)]
	 paste0(observed_variables, row, collapse = "")
	})



	used_strategies <- unique(apply(
		X = data_to_agg,
		MARGIN = 1,
		FUN = function(row){
			strategy <- paste(variables[!row == ""],collapse = "")
			return(strategy)
		}))


	observed_events <- table(observed_events)

	unobserved_event_labels <- possible_events[!possible_events %in% names(observed_events)]

	unused_strategies <- possible_strategies[!possible_strategies %in% used_strategies]

	unobserved_events <- rep(0,length(unobserved_event_labels))

	names(unobserved_events) <- unobserved_event_labels

	data_events <- data.frame(
		event = names(c(observed_events,unobserved_events)),
		count = c(observed_events,unobserved_events),
		strategy = rep(possible_strategies, i_strategy),
		row.names = NULL,
		stringsAsFactors = FALSE
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
#' @param pcm A dag created by make_dag()
#'
#' @export
#'
get_indicator_matrix  <- function(pcm){


	possible_data <-	get_possible_data(pcm)
	nodes <- names(possible_data)
	nodal_types <- get_nodal_types(pcm)
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



#' Prepare data for stan
#'
#' Create a list containing the data to be passed to stan
#'
#' @param pcm A dag created by \code{make_dag}
#' @param data A data frame with observations
#' @param lambdas_prior A vector containg priors for lambda
#' @param P A matrix mapping parameters (rows) to types (columns). If not provided, defaults one parameter per nodal type.
#' @return a list
#' @keywords internal
#'
make_gbiqq_data <- function(pcm, data, lambdas_prior = NULL, P = NULL ){

	dag <- pcm$dag
	if(is.null(P)) P <- get_indicator_matrix(pcm)

	n_params <- nrow(P)

	if(is.null(lambdas_prior)) lambdas_prior <- rep(1, n_params)

	inverted_P <- 1-P
	A <- get_ambiguities_matrix(pcm)
	likelihood_helpers <- get_likelihood_helpers(pcm)
	A_w <- likelihood_helpers$A_w


	data_events<-  get_data_events(data, pcm)$data_events
	A_w<- A_w[data_events$count != 0,]
	data_events <- data_events[data_events$count != 0,]
  strategies <- data_events$strategy
  n_strategies <- length(unique(strategies))
  w_starts <-  which(!duplicated(  strategies))
  k <- length(strategies)
  w_ends <- c(w_starts[2:n_strategies], k )
  if(n_strategies< 2){
  	w_ends <- k
  }


  possible_data <-   get_max_possible_data(pcm)
  P.lambdas <- P*lambdas_prior   +	1 - P
  prob_of_types <- apply(P.lambdas, 2, prod)
  w <- A %*% prob_of_types # Prob of (fundamental) data realization
  w_full <- A_w %*% w
  n_types_each <- sapply(gbiqq:::get_nodal_types(pcm), length)
  n_vars <- length(get_variables(pcm))
  l_ends <- cumsum(n_types_each)
  l_starts <- c(1, l_ends[1:(n_vars-1)] + 1)


	stan_data <- 	list(n_vars = n_vars,
										 n_nodal_types = nrow(P),
										 n_types = ncol(P),
										 n_types_each = n_types_each,
										 n_data = nrow( possible_data),
										 n_events = nrow(A_w),
										 n_strategies = n_strategies,
										 lambdas_prior = lambdas_prior,
										 l_starts = l_starts,
										 l_ends = l_ends,
										 strategy_starts = as.array(w_starts),
										 strategy_ends = as.array(w_ends),
										 P = P,
										 inverted_P = inverted_P,
										 A = A,
										 A_w= A_w,
										 Y = data_events$count)
}



#'
#' get_possible_data_internal
#'
#' @param pcm A dag created by \code{make_dag}
#' @return a list
#' @keywords internal
#'
get_possible_data_internal <- function(pcm, collapse = TRUE){
  nodal_types <- gbiqq:::get_nodal_types(pcm)
	var_names <-  names(nodal_types)
	t_nodal_types <- lapply(1:length(nodal_types),function(i){ gsub(var_names[i], "", nodal_types[[i]])} )
  if(!collapse){
	t_nodal_types <- lapply(1:length(t_nodal_types), function(i){
		variable <- var_names[i]
		var_parents <- parents[variable][[1]]
		var_variables <- c(var_parents,variable)
		out <- t_nodal_types[[i]]
	  out <- t(sapply(strsplit(out,""), as.numeric))
	  if(nrow(out) == 1) out <- t(out)

    colnames(out) <- var_variables
    out
	})}
	names(t_nodal_types) <- var_names
	t_nodal_types
 }




#' gbiqq-internal
#'
#' @importFrom Rcpp cpp_object_initializer
#' @export
#'
#'
cpp_object_initializer <- cpp_object_initializer
