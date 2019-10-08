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

	if(!is.null(vars[[1]])){
	 l_probs <- ifelse(!is.null(probs[[1]]), length(probs), length(vars))
	 l_subsets <- ifelse(!is.null(subsets[[1]]), length(subsets), length(vars))
	 if(any(!duplicated(c(length(vars), length(vars), l_probs,  l_subsets))[2:4]))
	 	stop("If specified, vars, probs, subsets, should have the same length")
  }

	if(!is.null(n) && length(n)!=length(vars)) stop("If specified, n should be the same length as vars")
	if(!is.null(n) && !is.null(probs)) warning("Both `n` and `prob` specified. `n` overrides `probs`.")

	if(is.null(complete_data)) {
		if(is.null(parameters)) {
			if(is.null(model$parameters)) message("parameters not provided")
			parameters <- draw_parameters(model, using = using)
			}
	    complete_data <- simulate_data(model, n = n_obs, parameters = parameters)
	 }

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

