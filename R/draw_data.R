#' Draw parameters
#' Default behavior takes a random draw of parameters from priors; can also draw from posteriors, or return pre defined parameters.
#'
#' @param model A model created by make_model()
#' @param using String indicating whether to use `priors`, `posteriors` or `parameters`
#' @importFrom gtools rdirichlet
#' @export
#' @examples
#' draw_parameters(model = model)
#'
draw_parameters <- function(model, using = NULL){

	if(is.null(using)) using <- "priors"

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
	if(using == "parameters" & is.null(parameters)) parameters <- draw_parameters(model, using = using)
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
#' draw_type_prob_multiple(model, using = "posteriors", n_draws = 3)
#' draw_type_prob_multiple(model, using = "parameters", n_draws = 3)

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
#' draw_event_prob(model = model, using = "parameters")
draw_event_prob <- function(model,
														P = NULL,
														A = NULL,
														parameters = NULL,
														type_prob = NULL,
														using = NULL){

	if(is.null(parameters)) {parameters <- draw_parameters(model, using = using)}

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
#' simulate_data(model, data_events = data_events)

simulate_data <- function(model,
													n = 1,
													data_events = NULL,
													parameters = NULL,
													using = "parameters"){

	if(!is.null(using)) if(using == "parameters" & is.null(parameters)) {
	if(is.null(model$parameters)) stop("parameters not provided")}

	# Data drawn here
	if(is.null(data_events)) data_events <- draw_data_events(model, n = n, parameters = parameters, using = using)

	# The rest is reshaping
	vars <- model$variables
	df <- merge(all_data_types(model), data_events, by.x = "event")
	xx  <- unlist(sapply(1:nrow(df), function(i) replicate(df[i,ncol(df)], df[i, vars])))
	out <- data.frame(matrix(xx, ncol = length(vars), byrow = TRUE))
	names(out) <- vars

	out

 }



