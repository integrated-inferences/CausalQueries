#' Observe data, given a strategy
#'
#' @param complete_data A data.frame. Data observed and unobserved.
#' @param observed A data.frame. Data observed.
#' @param vars_to_observe  A list of variables to observe.
#' @param prob A scalar. Observation probability.
#' @param m Number of units to observe; if specified, \code{m} overrides \code{prob}.
#' @param subset A logical statement that can be applied to rows of complete data. For instance observation fo some variables might depend on observed values of other variables; or observation may only be sought if data not already observed!
#'
#' @export
#' @examples
#' model <- make_model("X -> Y")
#' df <- simulate_data(model, n = 8)
#' # Observe X values only
#' observe(complete_data = df, vars_to_observe = "X")
#' # Observe half the Y values for cases with observed X = 1
#' observe(complete_data = df,
#'      observed = observe(complete_data = df, vars_to_observe = "X"),
#'      vars_to_observe = "Y", prob = .5,
#'      subset = "X==1")

# A strategy consists of a. names of types to reveal  b. number of these to reveal c. subset from which to reveal them

observe <- function(complete_data,
										observed = NULL,
										vars_to_observe = NULL,
										prob = 1,
										m = NULL,
										subset = NULL){

	if(is.null(observed)) {observed <- complete_data; observed[,] <- FALSE}
	if(is.null(vars_to_observe)) vars_to_observe <- names(complete_data)

	observed_data <- complete_data
	observed_data[!observed] <- NA

	# Handle cases with no subsetting; where condition is empty, and when the condition is satisfied

	if(is.null(subset)){ observed[, vars_to_observe] <- TRUE

	}  else {

		strata <- with(observed_data, eval(parse(text = subset)))

		if(max(strata) == 1){

			if(!is.null(m)) prob <- min(1, m/sum(strata))   # If m is specified, use this to extent possible

			show <- randomizr::strata_rs(strata = strata,
																	 strata_prob = c(0, prob)) == 1
			observed[show, vars_to_observe] <- TRUE
		}}

	observed
}

#' Data Strategy
#' @param model A causal model as created by \code{make_model}
#' @param n_obs Scalar giving number of observations in \code{complete_data}
#' @param parameters A specific parameter vector. If not provided parameters are drawn using `using` and `draw_parameters`
#' @param using String indicating whether to use `priors`, `posteriors` or `parameters`
#' @param n List indicating number of observations to be observed at each step
#' @param vars List indicating number which variables to be observed at each step
#' @param probs List indicating observation probabilities at each step
#' @param subsets List indicating strata within which observations are to be observed at each step
#' @param complete_data A dataset with complete observations. Optional.
#' @export
#' @examples
#' # A strategy in which X, Y are observed for sure and M is observed
#' # with 50% probability for X=1, Y=0 cases
#' model <- make_model("X -> M -> Y")
#' data_strategy(
#'   model,
#'   n_obs = 8,
#'   n = NULL,
#'   vars = list(c("X", "Y"), "M"),
#'   probs = list(1, .5),
#'   subsets = list(NULL, "X==1 & Y==0"))

data_strategy <- function(model,
													complete_data = NULL,
													n_obs   = NULL,
													parameters = NULL,
													using = "priors",
													n       = NULL,  # n at each step
													vars    = list(NULL),
													probs   = list(NULL),
													subsets = list(NULL)){


	if(!all.equal(length(vars), length(probs),  length(subsets))) stop(
		"vars, probs, subsets, should have the same length")
	if(!is.null(n) && length(n)!=length(vars)) stop("If specified, n should be the same length as vars")
	if(!is.null(n) && !is.null(probs)) warning("Both `n` and `prob` specified. `n` overrides `probs`.")

	if(is.null(complete_data)) {
		if(is.null(parameters)) {
			if(is.null(model$parameters)) message("parameters not provided")
			parameters <- draw_parameters(model, using = using)}
	    complete_data <- simulate_data(model, n = n_obs, parameters = parameters)}

	observed <- complete_data

	observed[,] <- FALSE

	# Default behavior is to return complete data -- triggered if first strategy step has vars =  null
	if(is.null(vars[[1]])) return(complete_data)

	# Otherwise work through strategies

	j = 1
	while(j <= length(vars)) {
		ifelse(!is.null(n) && !is.null(probs),
					 {name <- "n"; value = n[[j]]},
					 {name <- "prob"; value = probs[[j]]})
		if(!is.null(subsets[[j]])) {given <- paste0(", given ", subsets[[j]])
		} else { given <- NULL}
		description <- paste0("Step ", j, ": Observe ",
													paste(vars[[j]], collapse = ", "),
													" (", name, " = ", value, ")", given, ".\n")
		observed <- observe(complete_data,
												observed = observed,
												vars_to_observe = vars[[j]],
												prob = probs[[j]],
												m = n[[j]],
												subset = subsets[[j]])
		j = j + 1
		cat(description)
	}

	observed_data <- complete_data
	observed_data[!observed] <- NA
	observed_data
}

