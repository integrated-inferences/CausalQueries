
#' Get Possible Data
#'
#' Creates a list of all possible data realizations for each node given each possible parent value
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

	possible_data_list <- list()
	for (variable in variables) {
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


#' Summarize data events
#'
#' Take a dataframe and return compact dataframe of event types and strategies.
#'
#' @param data A data.frame of variables that can take three values: 0, 1, and NA
#' @param model A model created by make_model()
#'
#' @export
#'
#' @return A vector of data events
get_data_events <- function(data, model){

	likelihood_helpers  <- get_likelihood_helpers(model)
	possible_events     <- likelihood_helpers$possible_events
	possible_strategies <- names(likelihood_helpers$w_starts)
	variables           <- model$variables
	i_strategy          <- likelihood_helpers$w_ends - likelihood_helpers$w_starts +1


	if(!all(variables %in% names(data))){stop("Could not find all of the variables in the DAG in
																						the data you provided.\nPlease double-check variable
																						names and try again.")}

	revealed_data <- gbiqq:::reveal_outcomes(model)
	data <- data[,variables]


	# Stop if observed data is not in conflict with restrictions
	inconsistencies <- !apply(data, 1, function(observed){
		any(apply(revealed_data, 1, function(possible){
			!any(possible[!is.na(observed)] != observed[!is.na(observed)])}
		))
	})

	if(any(inconsistencies)){
		message(paste("Observations are not consistent with restrictions in", sum(!inconsistencies), "cases"))
		data <- data[!inconsistencies, ]
	}

	# replace "" with na and remove rows where all values are na
	data <- data[!apply(data, 1,  function(x) all(is.na(x) | x == "")),]
	data[is.na(data)] <- ""


	# Data events dataframe
	data_type <- apply(X = data, MARGIN = 1, FUN = function(row){
		paste0(variables[!(row == "")],row[!(row == "")], collapse = "")})

	data_events <- data.frame(event = possible_events,
														strategy   = rep(possible_strategies, i_strategy),
														count = 0,
														stringsAsFactors = FALSE,
														row.names = NULL)

	data_events$count <- sapply(data_events$event, function(j) sum(data_type == paste(j)))

	# Output
	list(
		data_events       = data_events,
		observed_events   = with(data_events, unique(event[count>0])),
		unobserved_events = with(data_events, unique(event[count==0])),
		used_strategies   = with(data_events, unique(strategy[count>0])),
		unused_strategies = with(data_events, unique(strategy[count==0]))
	)

}


#'
#' get_possible_data_internal
#'
#' @param model A model created by \code{make_model}
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
#' @param A model created by make_model()
#'
#' @export
#'
#' @return A data frame
#'
get_max_possible_data <- function(model) {
	dag <- model$dag
	max_possible_data <-
		Reduce(f = merge,
					 x = gbiqq:::get_possible_data_internal(model, collapse = FALSE))
	variables <- 	c(attr(model, "exogenous_variables"),
									attr(model, "endogenous_variables"))
	max_possible_data <- max_possible_data[variables]

	max_possible_data <-
		max_possible_data[do.call("order", as.list(max_possible_data[,rev(names(max_possible_data))])),]

	rownames(max_possible_data) <- 1:nrow(max_possible_data)

	return(max_possible_data)
}


#' Prepare data for stan
#'
#' Create a list containing the data to be passed to stan
#'
#' @param model A model created by \code{make_model}
#' @param data A data frame with observations
#' @param P A matrix mapping parameters (rows) to types (columns). If not provided, defaults one parameter per nodal type.
#' @return a list
#' @export
#' @examples
#' model <- make_model(add_edges(parent = "X", children = "Y"))
#' data   <- draw_data(model, n = 6)
#' make_gbiqq_data(model, data)
#' data[1,1]   <- NA
#' make_gbiqq_data(model, data)
#'
#'
make_gbiqq_data <- function(model, data){

	P                  <- get_parameter_matrix(model)
	param_set          <- attr(P, "param_set")
	model$priors <- get_priors(model)
	if(length(model$priors) != length(param_set)) stop("priors should have same length as parameter set")
	param_sets         <- unique(param_set)
	n_param_sets       <- length(param_sets)
	data_events        <- trim_strategies(model, data)
	inverted_P         <- 1-P
	A_w                <- (get_likelihood_helpers(model)$A_w)[data_events$event, ]
	strategies         <- data_events$strategy
	n_strategies       <- length(unique(strategies))
	w_starts           <- which(!duplicated(strategies))
	k                  <- length(strategies)
	w_ends             <- if(n_strategies < 2) k else c(w_starts[2:n_strategies]-1, k)
	n_param_each       <- sapply(param_sets, function(j) sum(param_set ==j))
	l_ends             <- cumsum(n_param_each)
	l_starts           <- c(1, l_ends[1:(n_param_sets-1)] + 1)

 list(n_params        = nrow(P),
			n_param_sets    = n_param_sets,
			n_param_each    = n_param_each,
			l_starts        = l_starts,
			l_ends          = l_ends,
			lambdas_prior   = model$priors,
			n_types         = ncol(P),
			n_data          = nrow(get_max_possible_data(model)),
			n_events        = nrow(A_w),
			n_strategies    = n_strategies,
			strategy_starts = as.array(w_starts),
			strategy_ends   = as.array(w_ends),
			P               = P,
			inverted_P      = inverted_P,
			A               = get_ambiguities_matrix(model),
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
