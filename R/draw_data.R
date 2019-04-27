
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

#' Draw event probabilities
#'
# `draw_event_prob` draws event probability vector `w`  given a single realization of lambda, drawn from model priors
#'
#' @param model A model created by make_model()
#' @param P Parameter matrix, not required but may be provided to avoide repeated computation for simulations
#' @param A Ambiguity matrix, not required but may be provided to avoide repeated computation for simulations
#' @param lambda A specific parameter vector, lambda, may be provided, otherwise lambda is drawn from priors
#'
#' @export
#' @examples
#' model <- make_model(add_edges(parent = "X", children = c("Y")))
#' draw_event_prob(model = model)

draw_event_prob <- function(model, P = NULL, A = NULL, lambda = NULL){

	if(is.null(lambda)) lambda <- draw_lambda(model)
	if(is.null(P)) 	    P      <- get_indicator_matrix(model)
	if(is.null(A)) 	    A      <- get_ambiguities_matrix(model)

	# Type probabilities
	P.lambdas     <- P*lambda +	1 - P
	prob_of_types <- apply(P.lambdas, 2, prod)

	# Event probabilities
	A %*% prob_of_types # Prob of (fundamental) data realization

 }


#' Draw compact data
#'
# `draw_data` draws `n` events given event probabilities
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
#' draw_data(model = model)

draw_data <- function(model,
											n = 1,
											w = NULL,
                      P = NULL,
											A = NULL,
											lambda = NULL
											){

 if(is.null(w)){
 	if(is.null(P)) 	P <- get_indicator_matrix(model)
 	if(is.null(A)) 	A <- get_ambiguities_matrix(model)
 	w <- draw_event_prob(model, P, A, lambda = lambda)
 }

	# Draw events (Compact dataframe)
	data.frame(event = rownames(w), count = rmultinom(1, n, w))

	}


#' Generate full dataset from compact data
#'
#' @param model A model created by make_model()
#' @param n Number of observations
#' @param compact_df A compact dataframe compatible with model
#'
#' @export
#' @examples
#' model <- make_model(add_edges(parent = "X", children = c("Y")))
#' compact_df <- draw_data(model = model, n = 4)
#' compact_to_full(model, compact_df)

compact_to_full <- function(model, compact_df){

	df <- get_max_possible_data(model)

	if(nrow(df) != nrow(compact_df)) stop("nrow(df) is not equal to nrow(compact_df)")

	xx  <- unlist(sapply(1:nrow(df), function(i) replicate(compact_df[i, 2],df[i,])))
	out <- data.frame(matrix(xx, ncol = ncol(df), byrow = TRUE))
	names(out) <- names(df)
	out
 }
