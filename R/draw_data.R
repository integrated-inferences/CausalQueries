
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

	if(is.null(model$lambda_priors)) model <- set_priors(model)
	lambdas_prior <- model$lambda_priors
	variables     <- get_variables(model)

	# Draw lambda, given priors
	lambda <- unlist(sapply(variables, function(v){
		i <- which(startsWith(names(lambdas_prior), v))
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
	if(is.null(A)) 	    A      <- get_ambiguities_matrix(model)

	# Type probabilities
	if(is.null(type_prob)) {
	type_prob <- draw_type_prob(model = model, P = P, lambda = lambda)}

	# Event probabilities
	A %*% type_prob

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
#'
#' @export
#' @examples
#' model <- make_model(add_edges(parent = "X", children = c("Y")))
#' data_events <- draw_data_events(model = model, n = 4)
#' draw_data(model, data_events = data_events)

draw_data <- function(model, n = 1, data_events = NULL){

	if(is.null(data_events)) data_events <- draw_data_events(model, n = n)

	df <- get_max_possible_data(model)

	if(nrow(df) != nrow(data_events)) stop("nrow(df) is not equal to nrow(data_events)")

	xx  <- unlist(sapply(1:nrow(df), function(i) replicate(data_events[i, 2],df[i,])))
	out <- data.frame(matrix(xx, ncol = ncol(df), byrow = TRUE))
	names(out) <- names(df)
	out

 }
