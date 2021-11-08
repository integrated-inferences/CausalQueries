#' Make data
#'
#' @inheritParams CausalQueries_internal_inherit_params
#' @param param_type A character. String specifying type of parameters to make
#' ("flat", "prior_mean", "posterior_mean", "prior_draw", "posterior_draw", "define").
#' With param_type set to \code{define} use arguments to be passed to \code{make_priors};
#' otherwise \code{flat} sets equal probabilities on each nodal type in each parameter set;
#' \code{prior_mean}, \code{prior_draw}, \code{posterior_mean}, \code{posterior_draw}
#' take parameters as the means or as draws from the prior or posterior.
#' @param n Non negative integer. Number of observations. If not provided it is inferred from the  largest n_step.
#' @param n_steps A \code{list}. Number of observations to be observed at each step
#' @param nodes A \code{list}. Which nodes to be observed at each step. If NULL all nodes are observed.
#' @param probs A \code{list}. Observation probabilities at each step
#' @param subsets A \code{list}. Strata within which observations are to be observed at each step. TRUE for all, otherwise an expression that evaluates to a logical condition.
#' @param complete_data A \code{data.frame}. Dataset with complete observations. Optional.
#' @param verbose Logical. If TRUE prints step schedule.
#' @param ... additional arguments that can be passed to \code{link{make_parameters}}
#' @return A \code{data.frame} with simulated data.
#' @export
#' @details
#' Note that default behavior is not to take account of whether a node has already been observed when determining whether to select or not. One can however specifically request observation of nodes that have not been previously observed.
#' @examples
#'
#' # Simple draws
#' model <- make_model("X -> M -> Y")
#' make_data(model)
#' make_data(model, n = 3, nodes = c("X","Y"))
#' make_data(model, n = 3, param_type = "prior_draw")
#' make_data(model, n = 10, param_type = "define", parameters =  0:9)
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
#'   subsets = list(TRUE, "X==1 & Y==0"))
#'
#'# n not provided but inferred from largest n_step (not from sum of n_steps)
#' make_data(
#'   model,
#'   nodes = list(c("X", "Y"), "M"),
#'   n_steps = list(5, 2))
#'
#' # Wide then deep
#'   make_data(
#'   model,
#'   n = 8,
#'   nodes = list(c("X", "Y"), "M"),
#'   subsets = list(TRUE, "!is.na(X) & !is.na(Y)"),
#'   n_steps = list(6, 2))
#'
# # Look for X only where X has not already been observed
#'
#' make_data(
#'   model,
#'   n = 8,
#'   nodes = list(c("X", "Y"), c("X", "M")),
#'   subsets = list(TRUE, "is.na(X)"),
#'   n_steps = list(3, 2))
#'
#'# Example with probabilities at each step
#'
#'make_data(
#'   model,
#'   n = 8,
#'   nodes = list(c("X", "Y"), c("X", "M")),
#'   subsets = list(TRUE, "is.na(X)"),
#'   probs = list(.5, .2))
#'

make_data <- function(
	model,
	n   = NULL,
	parameters = NULL,
	param_type = NULL,
	nodes    = NULL,  # nodes revealed at each step
	n_steps = NULL,         # n at each step
	probs   = NULL,            # probs at each step
	subsets = TRUE,         # subsets at each step
	complete_data = NULL,
	verbose = TRUE,
	...){

  # n_step, n consistency
  if(!is.null(n)) n_check(n)
  if(!is.null(n_steps) & !is.null(n)) if(max(n_steps %>% unlist) > n) stop("n_step larger than n")
  if(!is.null(n_steps) & is.null(n)) n <- max(n_steps %>% unlist)
  if(is.null(n_steps) & is.null(n)) n <- 1

	# n_steps and probs reconciliation
	if(!is.null(n_steps) & !is.null(probs)) warning("Both `n_steps` and `prob` specified. `n_steps` overrides `probs`.")
  if(is.null(n_steps) & is.null(probs)) n_steps <- n
  if(is.null(n_steps)) n_steps <- NA
  if(is.null(probs))   probs <- 1

	# Check that parameters sum to 1 in each param_set
	if(!is.null(parameters)) parameters <- clean_param_vector(model, parameters)

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
	if(all(model$nodes %in% nodes[[1]]))
		if(probs[1]==1 || n_steps[1]==n) return(complete_data)

	# Otherwise, gradually reveal

	# Check length consistency
	roster <- tibble(node_names = lapply(nodes, paste, collapse = ", ") %>% unlist,
	                 nodes = nodes, n_steps = n_steps %>% unlist, probs = probs%>% unlist, subsets = subsets%>% unlist)
	if(verbose) print(roster)

	observed <- complete_data

	observed[,] <- FALSE

	# Go step by step
	j = 1
	while(j <= length(nodes)) {

	  pars <- roster[j, ]

		observed <- observe_data(
			complete_data,
			observed = observed,
			nodes_to_observe = pars$nodes %>% unlist,
			prob = pars$probs,
			m    = pars$n_steps,
			subset = pars$subsets)

		j = j + 1

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
#' @param subset A character.  Logical statement that can be applied to rows of complete data. For instance observation for some nodes might depend on observed values of other nodes; or observation may only be sought if data not already observed!
#' @return A \code{data.frame} with logical values indicating which nodes to observe in each row of `complete_data`.
#' @importFrom stats runif
#' @importFrom dplyr tibble
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
												 subset = TRUE){

	if(is.null(observed)) {observed <- complete_data; observed[,] <- FALSE}
	if(is.null(nodes_to_observe)) nodes_to_observe <- names(complete_data)
	if(is.null(m)) m <- NA
	# Prep observed data dataframe
	observed_data <- complete_data
	observed_data[!observed] <- NA

	# Within which subset to reveal?
	if(!is.logical(subset) & subset != "TRUE") {
	  sub <- with(observed_data, eval(parse(text = subset)))
	} else {
	  sub <- rep(TRUE, nrow(observed_data))
	}

	if(!any(sub)) message("Empty subset")

	# Target to reveal
	if(is.na(m)){
	  E <- prob*sum(sub) # Expected number selected
	  m <- floor(E)  + (runif(1) <  E - floor(E)) # Get best m
	}

	observed[sample((1:length(sub))[sub], m), nodes_to_observe] <- TRUE

	observed
}


#' Generate full dataset
#'
#' @inheritParams CausalQueries_internal_inherit_params
#' @param n An integer. Number of observations.
#' @param parameters A numeric vector. Values of parameters may be specified. By default, parameters is drawn from priors.
#' @param param_type A character. String specifying type of parameters to make ("flat", "prior_mean", "posterior_mean", "prior_draw", "posterior_draw", "define). With param_type set to \code{define} use arguments to be passed to \code{make_priors}; otherwise \code{flat} sets equal probabilities on each nodal type in each parameter set; \code{prior_mean}, \code{prior_draw}, \code{posterior_mean}, \code{posterior_draw} take parameters as the means or as draws from the prior or posterior.
#' @param w Vector of event probabilities can be provided directly. This is useful for speed for repeated data draws.
#' @param P A \code{matrix}. Parameter matrix that can be used to generate w if w is not provided
#' @param A A \code{matrix}. Ambiguity matrix that can be used to generate w if w is not provided
#' @return A \code{data.frame} of simulated data.
#' @keywords internal
#'
#' @examples
#'
#' model <- make_model("X -> Y")
#'
#' # Simplest behavior uses by default  the parameter vector contained in model,
#' # which is flat by default:
#' CausalQueries:::make_data_single(model, n = 5)
#'
#' CausalQueries:::make_data_single(model, n = 5, param_type = "prior_draw")
#'
#' # Simulate multiple datasets is fastest if w is provided
#' w <- get_event_prob(model)
#' replicate(5, CausalQueries:::make_data_single(model, n = 5, w = w))
#'

make_data_single <- function(
	model,
	n = 1,
	parameters = NULL,
	param_type = NULL,
	w = NULL, P = NULL, A = NULL){

	# Check that parameters sum to 1 in each param_set
	# if(!is.null(parameters)) parameters <- clean_param_vector(model, parameters)

  # If parameters not provided, take from model
	if(is.null(parameters)) {
		if(!is.null(param_type)){
			parameters <- make_parameters(model, param_type = param_type)
		} else {
			parameters 	 <- get_parameters(model) }}

	# Generate event probabilities w if missing
	if(is.null(w)){
		# if(is.null(P)) 	P <- get_parameter_matrix(model)
		# if(is.null(A)) 	A <- get_ambiguities_matrix(model)
		# w <- get_event_prob(model, P, A, parameters = parameters)
		w <- get_event_prob(model, parameters = parameters, A = A, P = P)
		}

	# Data drawn here
	make_events(model, n = n,  parameters = parameters, w = w) %>%
		expand_data(model)

 }

#' simulate_data is an alias for make_data
#' @param ... arguments for \code{\link{make_model}}
#' @return A \code{data.frame} with simulated data.
#' @export
#' @examples
#' simulate_data(make_model("X->Y"))
simulate_data <- function(...) make_data(...)
