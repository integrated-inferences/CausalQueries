
#' This creates a list of all possible data realizations, for each child
#' @param model A probabilistic causal model created by \code{make_model()}
#' @param collapse whether to collapse possible data
#'
#' @export
#'
#' @return A list of all data realizations possible
get_possible_data <- function(model,
															collapse = TRUE){

	variables <- get_variables(model)
	parents <- get_parents(model)
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


#' Get the multinomial data events
#'
#' @param data A data.frame of variables that can take three values: 0, 1, and NA
#' @param model A dag created by make_dag()
#'
#' @export
#'
#' @return A vector of data events
get_data_events <- function(data, model){

	likelihood_helpers  <- get_likelihood_helpers(model)
	possible_events     <- likelihood_helpers$possible_events
	possible_strategies <- names(likelihood_helpers$w_starts)
	variables           <- get_variables(model)
	i_strategy          <- likelihood_helpers$w_ends - likelihood_helpers$w_starts +1


	if(!all(variables %in% names(data))){
		stop("Could not find all of the variables in the DAG in the data you provided.\nPlease double-check variable names and try again.")
	}

	revealed_data <- gbiqq:::reveal_outcomes(model)
	data <- data[,variables]
	data_to_agg <- data

	# Stop if observed data is not in conflict with restrictions
	inconsistencies <- !apply(data, 1, function(observed){
		  any(apply(revealed_data, 1, function(possible){
		  	!any(possible[!is.na(observed)] != observed[!is.na(observed)])}
		  	))
	})

	if(any(inconsistencies)){
		stop("Observations are not consistent with restrictions")
	}

	# replace "" with na and remove rows where all values are na

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


#'
#' get_possible_data_internal
#'
#' @param model A probabilistic causal model created by \code{make_model}
#' @return a list
#' @keywords internal
#'
get_possible_data_internal <- function(model, collapse = TRUE){

	revealed_data <- gbiqq:::reveal_outcomes(model)
	variables <- get_variables(model)
	parents <- get_parents(model)

	possible_data <- lapply(variables, function(variable){
		var_parents <- parents[variable][[1]]
		var_variables <- c(var_parents,variable)
		out <- revealed_data[,var_variables]
		duplicates <- duplicated(out)
		if(length(	var_variables) == 1) {
			out <- matrix(as.double(out[!duplicates]), ncol = length(var_variables))
		} else {
			out <- out[!duplicates, ]
		}
		colnames(out) <- var_variables
		out
	})
	names(possible_data) <- variables
	if(collapse){
		possible_data <- lapply(variables, function(variable){
			mat <- possible_data[[variable]]
			apply(mat, 1, function(x) paste0(variable, x, collapse = ""))
		})
	}

	names(possible_data) <- variables
	possible_data
}





#' Get data frame with all possible data combinations
#'
#' @param A probabilistic causal model created by make_model()
#'
#' @export
#'
#' @return A data frame
#'
get_max_possible_data <- function(model) {
	dag <- model$dag
	max_possible_data <-
		#Reduce(f = merge,
		#			 x = get_possible_data(dag, collapse = FALSE)[get_terminal_vars(dag)])
		Reduce(f = merge,
					 x = gbiqq:::get_possible_data_internal(model, collapse = FALSE))


	max_possible_data <- max_possible_data[get_variables(model)]

	max_possible_data <-
		max_possible_data[do.call("order", as.list(max_possible_data[,rev(names(max_possible_data))])),]

	rownames(max_possible_data) <- 1:nrow(max_possible_data)

	return(max_possible_data)
}


#' Prepare data for stan
#'
#' Create a list containing the data to be passed to stan
#'
#' @param model A probabilistic causal model created by \code{make_model}
#' @param data A data frame with observations
#' @param lambdas_prior A vector containg priors for lambda
#' @param P A matrix mapping parameters (rows) to types (columns). If not provided, defaults one parameter per nodal type.
#' @return a list
#' @export
#' @examples
#' model <- make_model(add_edges(parent = "X", children = "Y"))
#' data   <- draw_data(model, n = 3)
#' make_gbiqq_data(model, data)
#'
#'
make_gbiqq_data <- function(model, data){

	data_events        <- trim_strategies(model, data)
	P                  <- get_parameter_matrix(model)
	inverted_P         <- 1-P
	n_params           <- nrow(P)
  lambdas_prior      <- model$lambda_priors
	A                  <- get_ambiguities_matrix(model)
	A_w                <- (get_likelihood_helpers(model)$A_w)[data_events$event, ]
	strategies         <- data_events$strategy
	n_strategies       <- length(unique(strategies))
	w_starts           <-  which(!duplicated(strategies))
	k                  <- length(strategies)
	w_ends             <- c(w_starts[2:n_strategies], k )
	if(n_strategies < 2) w_ends <- k
  variables          <- get_variables(model)
  possible_data      <- get_max_possible_data(model)
  n_types_each       <- sapply(gbiqq:::get_nodal_types(model), length)
  n_vars             <- length(get_variables(model))
  l_ends             <- cumsum(n_types_each)
  l_starts           <- c(1, l_ends[1:(n_vars-1)] + 1)

 list(n_vars          = n_vars,
			n_nodal_types   = nrow(P),
			n_types         = ncol(P),
			n_types_each    = n_types_each,
			n_data          = nrow(possible_data),
			n_events        = nrow(A_w),
			n_strategies    = n_strategies,
			lambdas_prior   = lambdas_prior,
			l_starts        = l_starts,
			l_ends          = l_ends,
			strategy_starts = as.array(w_starts),
			strategy_ends   = as.array(w_ends),
			P               = P,
			inverted_P      = inverted_P,
			A               = A,
			A_w             = A_w,
			Y               = data_events$count)
}

#' gbiqq-internal
#'
#' @importFrom Rcpp cpp_object_initializer
#' @export
#'
#'
cpp_object_initializer <- cpp_object_initializer
