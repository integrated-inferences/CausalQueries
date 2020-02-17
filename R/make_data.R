#' Make data
#'
#' @inheritParams gbiqq_internal_inherit_params
#' @param param_type A character. String specifying type of parameters to make
#' ("flat", "prior_mean", "posterior_mean", "prior_draw", "posterior_draw", "define").
#' With param_type set to \code{define} use arguments to be passed to \code{make_priors};
#' otherwise \code{flat} sets equal probabilities on each nodal type in each parameter set;
#' \code{prior_mean}, \code{prior_draw}, \code{posterior_mean}, \code{posterior_draw}
#' take parameters as the means or as draws from the prior or posterior.
#' @param n Non negative integer. Number of observations.
#' @param n_steps A \code{list}. Number of observations to be observed at each step
#' @param nodes A \code{list}. Which nodes to be observed at each step
#' @param probs A \code{list}. Observation probabilities at each step
#' @param subsets A \code{list}. Strata within which observations are to be observed at each step
#' @param complete_data A \code{data.frame}. Dataset with complete observations. Optional.
#' @param ... additional arguments that can be passed to \code{link{make_parameters}}
#' @return A \code{data.frame} simulated data.
#' @importFrom randomizr strata_rs
#' @export
#'
#' @examples
#'
#'
#' # Simple draws
#' model <- make_model("X -> M -> Y")
#' make_data(model)
#' make_data(model, n = 3, nodes = c("X","Y"))
#' make_data(model, n = 3, param_type = "prior_draw")
#' make_data(model, n = 10, param_type = "define", alpha =  0:9)
#'
#' # Data Strategies
#' # A strategy in which X, Y are observed for sure and M is observed
#' # with 50% probability for X=1, Y=0 cases
#'
#' model <- make_model("X -> M -> Y")
#' make_data(
#'   model,
#'   n = 8,
#'   nodes = list(c("X", "Y"), "M"),
#'   probs = list(1, .5),
#'   subsets = list(NULL, "X==1 & Y==0"))
#'
#' # Simulate multiple datasets is fastest if w is provided
#' model <- make_model("X -> Y")
#' w <- get_event_prob(model)
#' replicate(5, make_data_single(model, n = 5, w = w))
#'

make_data <- function(
	model,
	n   = 1,
	parameters = NULL,
	param_type = NULL,
	nodes    = NULL,  # nodes revealed at each step
	n_steps = NULL,         # n at each step
	probs   = NULL,            # probs at each step
	subsets = TRUE,         # subsets at each step
	complete_data = NULL,
	...){

	# check n input
	n_check(n)

	# n_steps and probs reconciliation
	if(!is.null(n_steps) & !is.null(probs)) warning("Both `n_steps` and `prob` specified. `n_steps` overrides `probs`.")
	if(is.null(probs)) probs <- 1

		# Check that parameters sum to 1 in each param_set
	if(!is.null(parameters)) parameters <- gbiqq:::clean_param_vector(model, parameters)

	# If parameters not provided, make or take from model
	if(is.null(parameters)) {
		if(!is.null(param_type)){
			parameters <- make_parameters(model, param_type = param_type, ...)
		} else {
			parameters 	 <- get_parameters(model) }}


	# Check nodes
	if(is.null(nodes))  nodes <- list(model$nodes)
	if(!is.list(nodes)) nodes <- list(nodes) # Lets one step nodes be provided as vector
	if(!(all(unlist(nodes) %in% model$nodes))) stop("All listed nodes should be in model")

	# Complete data
	if(is.null(complete_data)) {
		complete_data <- make_data_single(model, n = n, parameters = parameters)
	}

	# Default behavior is to return complete data -- triggered if all data and all nodes sought in step 1
	if(all(model$nodes %in% nodes[[1]])){
		if(probs[1]==1 || n_steps[1]==n) return(complete_data)
	}


	# Otherwise, gradually reveal

	# Housekeeping

	# Check length consistency
	if(is.null(n_steps) & is.null(probs)) probs <- 1
	lengths = sapply(list(c(nodes, n_steps, probs, subsets)), length)
	if(sum(lengths >1)>1) if(sd(lengths[lengths >1]) > 0) stop("Incompatible lengths for
																														 nodes, n_steps, probs, subsets")

	if(length(nodes) >1) if(!((length(n_steps)== length(nodes)) || (length(probs)== length(nodes)) )){
  	stop("Multistep nodes requires compatible multistep n_steps or multistep probs")}


	observed <- complete_data

	observed[,] <- FALSE

	j = 1
	while(j <= length(nodes)) {
		ifelse(!is.null(n_steps) && !is.null(probs),
					 {name <- "n_steps"; value = n_steps[[j]]},
					 {name <- "prob"; value = probs[[j]]})
		if(!is.null(subsets[[j]])) {given <- paste0(", given ", subsets[[j]])
		} else { given <- NULL}
		description <- paste0("Step ", j, ": Observe ",
													paste(nodes[[j]], collapse = ", "),
													" (", name, " = ", value, ")", given, "\n") # , given, ".\n_steps")
		observed <- observe_data(
			complete_data,
			observed = observed,
			nodes_to_observe = nodes[[j]],
			prob = probs[[j]],
			m = n_steps[[j]],
			subset = subsets[[j]])
		j = j + 1
		cat(description)
	}

	observed_data <- complete_data
	observed_data[!observed] <- NA
	observed_data
}



#' Observe data, given a strategy
#'
#' @param complete_data A \code{data.frame}. Data observed and unobserved.
#' @param observed A \code{data.frame}. Data observed.
#' @param nodes_to_observe  A list. Nodes to observe.
#' @param prob A scalar. Observation probability.
#' @param m A integer. Number of units to observe; if specified, \code{m} overrides \code{prob}.
#' @param subset A character.  Logical statement that can be applied to rows of complete data. For instance observation fo some nodes might depend on observed values of other nodes; or observation may only be sought if data not already observed!
#'
#' @export
#' @examples
#' model <- make_model("X -> Y")
#' df <- simulate_data(model, n = 8)
#' # Observe X values only
#' observe_data(complete_data = df, nodes_to_observe = "X")
#' # Observe half the Y values for cases with observed X = 1
#' observe_data(complete_data = df,
#'      observed = observe_data(complete_data = df, nodes_to_observe = "X"),
#'      nodes_to_observe = "Y", prob = .5,
#'      subset = "X==1")

# A strategy consists of a. names of types to reveal  b. number of these to reveal c. subset from which to reveal them

observe_data <- function(complete_data,
												 observed = NULL,
												 nodes_to_observe = NULL,
												 prob = 1,
												 m = NULL,
												 subset = NULL){

	if(is.null(observed)) {observed <- complete_data; observed[,] <- FALSE}
	if(is.null(nodes_to_observe)) nodes_to_observe <- names(complete_data)

	observed_data <- complete_data
	observed_data[!observed] <- NA

	# Handle cases with no subsetting; where condition is empty, and when the condition is satisfied

	if(is.null(subset)){ observed[, nodes_to_observe] <- TRUE

	}  else {

		strata <- with(observed_data, eval(parse(text = subset)))

		if(max(strata) == 1){

			if(!is.null(m)) prob <- min(1, m/sum(strata))   # If m is specified, use this to extent possible

			if(prob == 1 && length(unique(strata))==1) { show <- TRUE } else {
				show <- strata_rs(strata = strata,
																		 strata_prob = c(0, prob)) == 1
			}
			observed[show, nodes_to_observe] <- TRUE
		}}

	observed
}


#' Generate full dataset
#'
#' @inheritParams gbiqq_internal_inherit_params
#' @param n An integer. Number of observations.
#' @param parameters A numeric vector. Values of parameters may be specified. By default, parameters is drawn from priors.
#' @param param_type A character. String specifying type of parameters to make ("flat", "prior_mean", "posterior_mean", "prior_draw", "posterior_draw", "define). With param_type set to \code{define} use arguments to be passed to \code{make_priors}; otherwise \code{flat} sets equal probabilities on each nodal type in each parameter set; \code{prior_mean}, \code{prior_draw}, \code{posterior_mean}, \code{posterior_draw} take parameters as the means or as draws from the prior or posterior.
#' @param w Vector of event probabilities can be provided directly. This is useful for speed for repeated data draws.
#' @param P A \code{matrix}. Parameter matrix that can be used to generate w if w is not provided
#' @param A A \code{matrix}. Ambiguity matrix that can be used to generate w if w is not provided
#' @return A \code{data.frame} of simulated data.
#' @export
#'
#' @examples
#'
#' model <- make_model("X -> Y")
#'
#' # Simplest behavior uses by default  the parameter vector contained in model,
#' # which is flat by default:
#' make_data_single(model, n = 5)
#'
#' make_data_single(model, n = 5, param_type = "prior_draw")
#'
#' # Simulate multiple datasets is fastest if w is provided
#' w <- get_event_prob(model)
#' replicate(5, make_data_single(model, n = 5, w = w))
#'

make_data_single <- function(

	model,
	n = 1,
	parameters = NULL,
	param_type = NULL,
	w = NULL, P = NULL, A = NULL){

	# Check that parameters sum to 1 in each param_set
	# if(!is.null(parameters)) parameters <- gbiqq:::clean_param_vector(model, parameters)

  # If parameters not provided, take from model
	if(is.null(parameters)) {
		if(!is.null(param_type)){
			parameters <- make_parameters(model, param_type = param_type)
		} else {
			parameters 	 <- get_parameters(model) }}

	# Generate event probabilities w if missing
	if(is.null(w)){
		if(is.null(P)) 	P <- get_parameter_matrix(model)
		if(is.null(A)) 	A <- get_ambiguities_matrix(model)
		w <- get_event_prob(model, P, A, parameters = parameters)}

	# Data drawn here
	simulate_events(model, n = n,  parameters = parameters, w = w, clean_params = FALSE) %>%
		expand_data(model)

 }

#' simulate_data is an alias for make_data
#' @param ... arguments for \code{\link{make_model}}
#' @export
#' @examples
#' simulate_data(make_model("X->Y"))
simulate_data <- function(...) make_data(...)
