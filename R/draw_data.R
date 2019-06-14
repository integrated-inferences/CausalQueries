
#' Draw parameters
#'
# `draw_lambda` draws a parameters vector given model priors
#'
#' @param model A model created by make_model()
#' @param using String indicating whether to use `priors`, `posteriors` or `parameters`
#' @importFrom gtools rdirichlet
#' @export
#' @examples
#' model <- make_model("X -> Y")
#' draw_lambda(model = model)

draw_lambda <- function(model, using = "priors"){

	if(!(using %in% c("priors", "posteriors", "parameters"))) stop(
		"`using` should be one of `priors`, `posteriors`, or `parameters`")

	if(using == "parameters") {if(is.null(model$parameters)) stop("No parameters provided")
		                         return(model$parameters)}

	if(using == "posteriors") {if(is.null(model$posterior)) stop("No posterior provided")
		param_dist <- rstan::extract(model$posterior, pars= "lambdas")$lambdas
		return(param_dist[sample(nrow(param_dist),1),])
	}

	if(is.null(model$priors)) {model <- set_priors(model)
	                                  message(paste("Priors missing from model. Generated on the fly."))
	}
	if(is.null(model$P)) {model <- set_parameter_matrix(model)
												message(paste("Parameter matrix missing from model. Generated on the fly."))
	}

	lambdas_prior <- model$priors
	param_set     <- attr(model$P, "param_set")

	if(length(lambdas_prior) != length(param_set)) stop("parameters priors should have same length as parameter set")

	param_sets         <- unique(param_set)

	# Draw parameters, given priors
	parameters <- unlist(sapply(param_sets, function(v){
		i <- which(startsWith(names(lambdas_prior), paste0(v,".")))
		rdirichlet(1, lambdas_prior[i])}))
	names(parameters) <- names(lambdas_prior)
	parameters
}

#' Draw type probabilities
#'
# `draw_type_prob` draws probability of vector of causal types  given a single realization of parameters, possibly drawn from model priors
#'
#' @param model A model created by make_model()
#' @param P Parameter matrix, not required but may be provided to avoide repeated computation for simulations
#' @param parameters A specific parameter vector, parameters, may be provided, otherwise parameters is drawn using `using` (either from priors, poseriors, or from model$parameters)
#' @param using String indicating whether to use `priors`, `posteriors` or `parameters`
#'
#' @export
#' @examples
#' model <- make_model("X -> Y")
#' draw_type_prob(model = model)

draw_type_prob <- function(model,
													 P = NULL,
													 parameters = NULL,
													 using = NULL ){

	if(!is.null(parameters)) using <- "parameters"
	if(using == "parameters" & is.null(parameters)) parameters <- draw_lambda(model, using = using)
	if(is.null(P)) 	    P      <- get_parameter_matrix(model)

	# Type probabilities
	P.lambdas     <- P*parameters +	1 - P
	apply(P.lambdas, 2, prod)

}

#' Draw matrix of type probabilities, before or after estimation
#'
#' @param model A model, normally containing a prior or a posterior distribution
#' @param using Character string indicating whether to use `priors`, `posteriors` or `parameters`
#' @param parameters A true parameter vector to be used instead of parameters attached to the model in case  `using` specifies `parameters`
#' @param n_draws If no prior distribution provided, generate prior distribution with n_draws draws
#' @param using String indicating whether to use `priors`, `posteriors` or `parameters`
#' @export
#' @examples
#' model <- make_model("X -> Y")
#' draw_type_prob_multiple(model, using = "priors", n_draws = 3)
#' draw_type_prob_multiple(model, using = "parameters", n_draws = 3)
#' \dontrun{
#' data <- simulate_data(model)
#' model <- gbiqq(model = model, data = data)
#' draw_type_prob_multiple(model, using = "posteriors", n_draws = 3)
#' }


draw_type_prob_multiple <- function(model,
																		parameters = NULL,
																		n_draws = 4000,
																		using = "priors"){

	if(using == "parameters") {
		if(is.null(model$parameters) & is.null(parameters)) stop("please provide parameters")
		if(is.null(parameters)) parameters <- model$parameters
		return(draw_type_prob(model, parameters = parameters, using = using))
	  }

	if(using == "priors"){
		if(is.null(model$prior_distribution)) {
			message("Prior distribution added to model")
			model <- set_prior_distribution(model, n_draws = n_draws)
		}
		param_dist <- model$prior_distribution
		}

	if(using == "posteriors"){
		if(is.null(model$posterior_distribution)) {
			stop("Model does not contain a posterior distribution")}
		param_dist <- rstan::extract(model$posterior, pars= "lambdas")$lambdas
	}

	apply(param_dist, 1, function(j) draw_type_prob(model, parameters = j))
}



#' Draw event probabilities
#'
# `draw_event_prob` draws event probability vector `w`  given a single realization of parameters
#'
#' @param model A model created by make_model()
#' @param P Parameter matrix, not required but may be provided to avoid repeated computation for simulations
#' @param A Ambiguity matrix, not required but may be provided to avoid repeated computation for simulations
#' @param parameters A specific parameter vector, parameters; if not  provided,  parameters is drawn from priors
#' @param vector of type probabilities; usually not required
#' @param using String indicating whether to use `priors`, `posteriors` or `parameters`
#'
#' @export
#' @examples
#' model <- make_model("X -> Y")
#' draw_event_prob(model = model)
draw_event_prob <- function(model,
														P = NULL,
														A = NULL,
														parameters = NULL,
														type_prob = NULL,
														using = NULL){

		if(is.null(parameters)) {
		if(is.null(model$parameters)) stop("Parameters not provided")
		parameters <- model$parameters }

	# Ambiguity matrix
	if(is.null(A)) 	    A <- get_ambiguities_matrix(model)

	# Type probabilities
	if(is.null(type_prob)) {
	type_prob <- draw_type_prob(model = model, P = P, parameters = parameters, using = using)}

	# Event probabilities  ## FLAG this is a hack for cases with only one possible data type
	if(ncol(A)==1) {out <- matrix(1); rownames(out) <- colnames(A); return(out)}

	t(A) %*% type_prob

 }


#' Draw compact data
#'
# `draw_data_events` draws `n` events given event probabilities
#'
#' @param model A model created by make_model()
#' @param n Number of observations
#' @param w Vector of event probabilities
#' @param P Optional parameter matrix: not required but may be provided to avoide repeated computation for simulations
#' @param A Optional ambiguity matrix: not required but may be provided to avoide repeated computation for simulations
#' @param parameters A specific parameter vector, parameters, may be provided, otherwise parameters is drawn from priors
#' @importFrom stats rmultinom
#' @param using String indicating whether to use `priors`, `posteriors` or `parameters`
#'
#' @export
#' @examples
#' model <- make_model("X -> Y")
#' draw_data_events(model = model)

draw_data_events <- function(model,
											n = 1,
											w = NULL,
                      P = NULL,
											A = NULL,
											parameters = NULL,
											using = NULL
											){

 if(is.null(w)){
 	if(is.null(P)) 	P <- get_parameter_matrix(model)
 	if(is.null(A)) 	A <- get_ambiguities_matrix(model)
 	w <- draw_event_prob(model, P, A, parameters = parameters, using = using)
 }

	# Draw events (Compact dataframe)
	data.frame(event = rownames(w), count = rmultinom(1, n, w))

	}


#' Generate full dataset, possibly from compact data
#'
#' @param model A model created by make_model()
#' @param n Number of observations
#' @param data_events A compact dataframe compatible with model
#' @param parameters A specific parameter vector, parameters, may be provided, otherwise parameters is drawn from priors
#' @param using String indicating whether to use `priors`, `posteriors` or `parameters`. Defaults here to "parameters."
#'
#' @export
#' @examples
#' model <- make_model("X -> Y")
#' data_events <- draw_data_events(model = model, n = 4)
#' draw_data(model, data_events = data_events)

simulate_data <- function(model,
													n = 1,
													data_events = NULL,
													parameters = NULL,
													using = "parameters"){

	if(using == "parameters" & is.null(parameters)) {
		if(is.null(model$parameters)) stop("parameters not provided")}

	# Data drawn here
	if(is.null(data_events)) data_events <- draw_data_events(model, n = n, parameters = parameters, using = using)

	# The rest is reshaping
	df <- get_max_possible_data(model)

	if(nrow(df) != nrow(data_events)) stop("nrow(df) is not equal to nrow(data_events)")

	xx  <- unlist(sapply(1:nrow(df), function(i) replicate(data_events[i, 2],df[i,])))
	out <- data.frame(matrix(xx, ncol = ncol(df), byrow = TRUE))
	names(out) <- names(df)

	out

 }


#' Observe data, given a strategy
#'
#' @param complete_data   Data observed and unobserved
#' @param observed_data   Data observed so far
#' @param vars_to_observe  A list of variables to observe
#' @param prob    Observation probability
#' @param n    Number of units to observe; if specified, \code{n} overrides \code{prob}
#' @param subset  A logical statement that can be applied to rows of complete data. For instance observation fo some variables might depend on observed values of other variables; or observation may only be sought if data not already observed!
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
#' @param using String indicating whether to use `priors`, `posteriors` or `parameters`
#' @export
#' @examples
#' # A strategy in which X, Y are observed for sure and M is observed with 50% probability for X=1, Y=0 cases
#' model <- make_model("X -> M -> Y")
#' model <- set_parameters(model, type = "flat")
#' data_strategy(
#'   model,
#'   n_obs = 8,
#'   n = NULL,
#'   vars = list(c("X", "Y"), "M"),
#'   probs = list(1, .5),
#'   subsets = list(NULL, "X==1 & Y==0"))

data_strategy <- function(model,
													parameters = NULL,
													n_obs   = NULL,
													n       = NULL,  # n at each step
													vars    = list(NULL),
													probs   = list(NULL),
													subsets = list(NULL),
													using = "priors"){

	if(is.null(parameters)) {
		if(is.null(model$parameters)) message("parameters not provided")
		parameters <- draw_lambda(model, using = using)}

	if(!all.equal(length(vars), length(probs),  length(subsets))) stop(
		"vars, probs, subsets, should have the same length")
	if(!is.null(n) && length(n)!=length(vars)) stop("If specified, n should be the same length as vars")
	if(!is.null(n) && !is.null(probs)) warning("Both `n` and `prob` specified. `n` overrides `probs`.")

	complete_data <- observed <- simulate_data(model, n = n_obs, parameter = parameters)
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


#' Encode data
#'
#' Takes data in long format, including NA values or blanks and returns vector with each row encoded as a data type.
#'
#' @param model A  model
#' @param data Data in long format
#' @export
#' @examples
#' model <- make_model("X -> Y")
#' data <- simulate_data(model, n = 4)
#' data[1,1] <- ""
#' data[3,2] <- NA
#'
#' encode_data(model, data)
encode_data <- function(model, data){
	data[data ==""] <- NA
	vars <- model$variables
	apply(data, MARGIN = 1, FUN = function(row){
		paste0(vars[!(is.na(row))],row[!(is.na(row))], collapse = "")})
}
