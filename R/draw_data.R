
#' Draw lambda
#'
# `draw_lambda` draws a lambda vector given model priors
#'
#' @param model A model created by make_model()
#' @importFrom gtools rdirichlet
#' @export
#' @examples
#' draw_lambda(model = model)

draw_lambda <- function(model){

	if(is.null(model$lambda_priors)) {model <- set_priors(model)
	                                  message(paste("Priors missing from model. Generated on the fly."))
	}
	if(is.null(model$P)) {model <- set_parameter_matrix(model)
												message(paste("Parameter matrix missing from model. Generated on the fly."))
	}

	lambdas_prior <- model$lambda_priors
	param_set     <- attr(model$P, "param_set")

	if(length(lambdas_prior) != length(param_set)) stop("lambda priors should have same length as parameter set")

	param_sets         <- unique(param_set)

	# Draw lambda, given priors
	lambda <- unlist(sapply(param_sets, function(v){
		i <- which(startsWith(names(lambdas_prior), paste0(v,".")))
		rdirichlet(1, lambdas_prior[i])}))
	names(lambda) <- names(lambdas_prior)
	lambda
}

#' Draw type probabilities
#'
# `draw_type_prob` draws probability of vector of causal types  given a single realization of lambda, drawn from model priors
#'
#' @param model A model created by make_model()
#' @param P Parameter matrix, not required but may be provided to avoide repeated computation for simulations
#' @param lambda A specific parameter vector, lambda, may be provided, otherwise lambda is drawn from priors
#'
#' @export
#' @examples
#' model <- make_model(add_edges(parent = "X", children = c("Y")))
#' draw_type_prob(model = model)

draw_type_prob <- function(model, P = NULL,  lambda = NULL){

	if(is.null(lambda)) lambda <- draw_lambda(model)
	if(is.null(P)) 	    P      <- get_parameter_matrix(model)

	# Type probabilities
	P.lambdas     <- P*lambda +	1 - P
	apply(P.lambdas, 2, prod)

}

#' Draw matrix of type probabilities, before or after estimation
#'
#' @param model A model, normally containing a prior or a posterior distribution
#' @param posterior if true use a posterior distribution, otherwise use the prior
#' @param n_draws If no prior distribution provided, generate prior distribution with n_draws draws
#' @export

draw_type_prob_multiple <- function(model, posterior = FALSE, n_draws = 4000){

	if(!posterior){
		if(is.null(model$prior_distribution)) {
			message("Prior distribution added to model")
			model <- set_prior_distribution(model, n_draws = n_draws)
		}
		lambdas <- model$prior_distribution
		}

	if(posterior){
		if(is.null(model$posterior_distribution)) {
			stop("Model does not contain a posterior distribution")}
		lambdas <- rstan::extract(model$posterior, pars= "lambdas")$lambdas
	}

	P  <- get_parameter_matrix(model)

	apply(lambdas, 1, function(j) draw_type_prob(model, P = P,  lambda = j))
}



#' Draw event probabilities
#'
# `draw_event_prob` draws event probability vector `w`  given a single realization of lambda, drawn from model priors
#'
#' @param model A model created by make_model()
#' @param P Parameter matrix, not required but may be provided to avoid repeated computation for simulations
#' @param A Ambiguity matrix, not required but may be provided to avoid repeated computation for simulations
#' @param lambda A specific parameter vector, lambda; if not  provided,  lambda is drawn from priors
#'
#' @export
#' @examples
#' model <- make_model(add_edges(parent = "X", children = c("Y")))
#' draw_event_prob(model = model)

draw_event_prob <- function(model, P = NULL, A = NULL, lambda = NULL, type_prob = NULL){

	# Ambiguity matrix
	if(is.null(A)) 	    A <- get_ambiguities_matrix(model)

	# Type probabilities
	if(is.null(type_prob)) {
	type_prob <- draw_type_prob(model = model, P = P, lambda = lambda)}

	# Event probabilities
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
#' @param lambda A specific parameter vector, lambda, may be provided, otherwise lambda is drawn from priors
#'
#' @export
#' @examples
#' model <- make_model(add_edges(parent = "X", children = c("Y")))
#' draw_data_events(model = model)

draw_data_events <- function(model,
											n = 1,
											w = NULL,
                      P = NULL,
											A = NULL,
											lambda = NULL
											){

 if(is.null(w)){
 	if(is.null(P)) 	P <- get_parameter_matrix(model)
 	if(is.null(A)) 	A <- get_ambiguities_matrix(model)
 	w <- draw_event_prob(model, P, A, lambda = lambda)
 }

	# Draw events (Compact dataframe)
	data.frame(event = rownames(w), count = rmultinom(1, n, w))

	}


#' Generate full dataset, possibly from compact data
#'
#' @param model A model created by make_model()
#' @param n Number of observations
#' @param data_events A compact dataframe compatible with model
#' @param lambda A specific parameter vector, lambda, may be provided, otherwise lambda is drawn from priors
#'
#' @export
#' @examples
#' model <- make_model(add_edges(parent = "X", children = c("Y")))
#' data_events <- draw_data_events(model = model, n = 4)
#' draw_data(model, data_events = data_events)

simulate_data <- function(model, n = 1, data_events = NULL, lambda = NULL, strategy = NULL){

	if(is.null(data_events)) data_events <- draw_data_events(model, n = n, lambda = lambda)

	df <- get_max_possible_data(model)

	if(nrow(df) != nrow(data_events)) stop("nrow(df) is not equal to nrow(data_events)")

	xx  <- unlist(sapply(1:nrow(df), function(i) replicate(data_events[i, 2],df[i,])))
	out <- data.frame(matrix(xx, ncol = ncol(df), byrow = TRUE))
	names(out) <- names(df)

	if(is.null(strategy)) return(out)

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
#' model <- make_model(add_edges(parent = "X", children = "Y"))
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
#' @export
#' @examples
#' model <- make_model("X" %->% "M", "M" %->% "Y")
#' data_strategy(
#'    model,
#'    n = 8,
#'    vars = list(c("X", "Y"), "M"),
#'    probs = list(1, .5),
#'    subsets = list(NULL, "X==1 & Y==0"))

data_strategy <- function(model,
	                  n,
										vars    = list(NULL),
										probs   = list(NULL),
										ms      = NULL,
										subsets = list(NULL)){

	if(!all.equal(length(vars), length(probs),  length(subsets))) stop(
		"vars, probs, subsets, should have the same length")
	if(!is.null(ms)) if(length(ms)!=length(vars)) stop("If specified, ms should be the same length as vars")

	complete_data <- observed <- simulate_data(model, n = n)
	observed[,] <- FALSE

	# Default behavior is to return complete data -- triggered if first strategy step has vars =  null
	if(is.null(vars[[1]])) return(complete_data)

	# Otherwise work through strategies

	j = 1
	while(j <= length(vars)) {
	observed <- observe(complete_data,
											observed = observed,
											vars_to_observe = vars[[j]],
											prob = probs[[j]],
											m = ms[[j]],
											subset = subsets[[j]])
	j = j + 1
	}

	observed_data <- complete_data
	observed_data[!observed] <- NA
	observed_data
}
