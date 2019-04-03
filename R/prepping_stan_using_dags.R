
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
	likelihood_helpers <- get_likelihood_helpers(dag)
	possible_events <- likelihood_helpers$possible_events
	possible_strategies <- names(likelihood_helpers$w_starts)
	variables <- get_variables(dag)
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



#' Prepare data for stan
#'
#' Create a list containing the data to be passed to stan
#'
#' @param dag A dag as created by \code{make_dag}
#' @param data A data frame with observations
#' @param lambdas_prior A vector containg priors for lambda
#' @return a list
#' @keywords internal
#'
make_gbiqq_data <- function(dag, data, lambdas_prior ){

	P <- get_indicator_matrix(dag )
	inverted_P <- 1-P
	A <- get_ambiguities_matrix(dag )
	likelihood_helpers <- get_likelihood_helpers(dag)
	A_w <- likelihood_helpers$A_w


	data_events<-  get_data_events(data, dag )$data_events
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


  possible_data <-   get_max_possible_data(dag )
  P.lambdas <- P*lambdas_prior   +	1 - P
  prob_of_types <- apply(P.lambdas, 2, prod)
  w <- A %*% prob_of_types # Prob of (fundamental) data realization
  w_full <- A_w %*% w
  n_types_each <- sapply(gbiqq:::get_nodal_types(dag), length)
  n_vars <- length(get_variables(dag ))
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
										 strategy_starts = w_starts,
										 strategy_ends = w_ends,
										 P = P,
										 inverted_P = inverted_P,
										 A = A,
										 A_w= A_w,
										 Y = get_data_events(data, dag )$data_events$count)
}

#' Fit stan model
#'
#' @param dag A dag as created by \code{make_dag}
#' @param data A data frame with observations
#' @param lambdas_prior A vector containg priors for lambda
#' @importFrom rstan stan
#' @importFrom Rcpp cpp_object_initializer
#' @export
#'
gbiqq <- function(dag, lambda_priors,  data, ...) {


simpler <- "data {

	int<lower=1> n_vars;
	int<lower=1> n_nodal_types;
	int<lower=1> n_types;
	int<lower=1> n_types_each[n_vars];
	int<lower=1> n_data;
	int<lower=1> n_events;
	int<lower=1> n_strategies;

	vector<lower=0>[n_nodal_types] lambdas_prior;
	int<lower=1> l_starts[n_vars];
	int<lower=1> l_ends[n_vars];
	int<lower=1> strategy_starts[n_strategies];
	int<lower=1> strategy_ends[n_strategies];

	vector[n_types] P[n_nodal_types] ;
	vector[n_types] inverted_P[n_nodal_types] ;
	matrix<lower=0,upper=1>[n_data, n_types] A;
	matrix<lower=0,upper=1>[n_events,n_data] A_w;
	int<lower=0,upper=2147483647> Y[n_events]; // incorrect dimensionality, only for the tryout

}

parameters {
	vector<lower=0>[n_nodal_types - n_vars] gamma;
}

transformed parameters {
	vector<lower=0>[n_nodal_types] lambdas;
	vector<lower=1>[ n_vars] sum_gammas;
	for (i in 1:n_vars) {

		sum_gammas[i] =
			1 + sum(gamma[(l_starts[i] - (i-1)):(l_ends[i] - i)]);

		lambdas[l_starts[i]:l_ends[i]] =
			append_row(1, gamma[(l_starts[i] - (i-1)):(l_ends[i] - i)]) / sum_gammas[i];

	}
}

model {
	vector[n_data] w;
	vector[n_events] w_full;
	vector[n_types] prob_of_types;
	vector[n_nodal_types] P_lambdas[n_types];

	for (i in 1:n_types) {
		for (j in 1:n_nodal_types) {
			P_lambdas[i, j] = P[j, i] .* lambdas[j] + inverted_P[j, i];
		}
		prob_of_types[i] = prod(P_lambdas[i]);
	}

	w = A * prob_of_types;
	w_full = A_w * w;


	target += gamma_lpdf(lambdas  | lambdas_prior, 1);
	for (i in 1:n_vars) {
		target += -n_types_each[i] * log(sum_gammas[i]);
	}

	for (i in 1:n_strategies) {
		target += multinomial_lpmf(
			Y[strategy_starts[i]:strategy_ends[i]] | w_full[strategy_starts[i]:strategy_ends[i]]);
	}
}
"


stan_data <- make_gbiqq_data(dag, data, lambda_priors)
fitted_model <-	stan(model_code = simpler,
	data = stan_data,  ...)

return(fitted_model)
}

#' gbiqq-internal
#'
#' @importFrom Rcpp cpp_object_initializer
#' @export
#'
#'
cpp_object_initializer <- cpp_object_initializer
