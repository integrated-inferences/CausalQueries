#' Set priors
#'
#' @param model A model created by make_model()
#' @param prior_distribution A character indicating the prior distribuiton
#' @param by_nodes the hyperparameters of the dirichlet distribution.
#' @param by_causal_types
#' Stipulated alpha values override prior_distribution
#' @export
#' @examples
#' XYmodel <- make_model("X -> Y")
#' #  Default sets all priors to 1
#' make_priors(model = XYmodel)
#' #  Set all priors to 0.5
#' make_priors(model = XYmodel, prior_distribution = "jeffreys")
#' #  Set all priors to infinity
#' make_priors(model = XYmodel, prior_distribution = "certainty")
#'
#'#  set all priors to 1 except for prior of nodal_type X0
#' make_priors(model = XYmodel, alphas = list(X = c(X0 = 2)))
#'#  specify priors for each of the nodal_types in model
#' make_priors(model = XYmodel,
#'            alphas = list(X = c(X0 = 2, X1 = 1),  Y = c(Y00 = 1, Y01 = 2, Y10 = 2, Y11 = 1)))
#' # set all priors to 10
#' make_priors(model = XYmodel, alphas =  10)
make_priors  <- function(model,  prior_distribution = "uniform", alphas = NULL){

	if(!is.null(prior_distribution)){
		if(!(prior_distribution %in% c("uniform", "jeffreys", "certainty")))
			stop("prior_distribution should be either 'uniform', 'jeffreys', or 'certainty'.")
	}

	# parameter housekeeping
	nodal_types_list        <- get_nodal_types(model) # change to nodal_types!
	nodal_types             <- unlist(get_nodal_types(model))
	n_params                <- length(nodal_types)
	param_set               <- unlist(sapply(1:length(nodal_types_list),
																					 function(i) rep(names(nodal_types_list)[i], length(nodal_types_list[[i]]))))
	param_sets              <- unique(param_set)
	n_param_sets            <- length(param_sets)
	par_names               <- paste0(param_set, ".", nodal_types)

	# alpha housekeeping
	alphas_vector <- unlist(alphas)
	n_alphas      <- length(alphas_vector)
	if(is.null(prior_distribution) & ((n_alphas!=1 & n_alphas!=n_params)))	stop(
		"if prior_distribution is not specified, alphas must contain either a value
		for each parameter or a single value to be assigned to all parameters.")
	alpha_names   <- names(alphas_vector)


	if(n_alphas == 1 & is.null(alpha_names)){
		alphas_vector <- rep(alphas, n_params)
		names(alphas_vector) <- alpha_names   <- par_names
	}


	# Check that alpha names match par names
	not_in_dag <- !alpha_names %in% par_names
	if(any(not_in_dag )){
		index <- which(not_in_dag)
		error_text	<- sapply(index, function(i){

			reversed_name <- sapply(lapply(strsplit(alpha_names[i], NULL), rev), paste, collapse = "")
			splitted_name <- unlist(  strsplit(sub("[.]", ";", reversed_name), ";"))

			paste0("\n variable ", splitted_name[2], " and corresponding nodal_type ",  splitted_name[1], " must match variables in dag and nodal_types syntax")

		})
		stop(	error_text )}

	# Check alpha strictly positive
	if(any(	alphas_vector  <= 0)) stop("alphas must be strictly positive real numbers")

	# Calculate lambda_priors
	if(!is.null(prior_distribution)){
		if(prior_distribution == "uniform")   lambda_priors <- rep(1, n_params )
		if(prior_distribution == "jeffreys")  lambda_priors <- rep(0.5, n_params )
		if(prior_distribution == "certainty") lambda_priors <- rep(10000, n_params )

	} else{
		if(n_alphas == 1 & is.null(alpha_names))   lambda_priors <- rep(alphas, n_params)
		if(n_alphas == n_params)                   lambda_priors <- alphas_vector}

	# Substitute alpha vector when provided
	names(lambda_priors)       <- par_names
	lambda_priors[alpha_names] <- alphas_vector

	# result
	lambda_priors
}



#' Set prior distribution
#'
#' @param model A model created by make_model()
#' @param lambda_priors Existing vector of Dirichlet hyperparameters
#' @param prior_distribution A character indicating the prior distribuiton
#' @param alphas Vector of hyperparameters of the dirichlet distribution to be passed to \code{make_priors}.
#'
#' @export

set_priors  <- function(model,
												lambda_priors = NULL,
												prior_distribution = "uniform",
												alphas = NULL) {

	if(is.null(lambda_priors)) lambda_priors <-
			make_priors(model,
									prior_distribution = prior_distribution,
									alphas = alphas)

	model$lambda_priors  <- lambda_priors
	message(paste("Priors attached to model"))
	model
}



#' Get prior distribution
#'
#' @param model A model created by make_model()
#' @param lambda_priors Vector of Dirichlet hyperparameters
#' @param prior_distribution A character indicating the prior distribuiton
#' @param alphas the hyperparameters of the dirichlet distribution.
#'
#' @export

get_priors  <- function(model,  lambda_priors = NULL, prior_distribution = "uniform", alphas = NULL) {

	if(!is.null(model$lambda_priors)) return(model$lambda_priors)

	message(paste("Priors missing from model. Generated on the fly."))
	make_priors(model, prior_distribution = prior_distribution, alphas = alphas)

}


#' Add prior distribution draws
#'
#' Add n_param x n_draws database of possible lambda draws to the model
#'
#' @param model A model generated by \code{make_model}
#' @param n_draws Number of draws
#' @export
#'
set_prior_distribution <- function(model, n_draws = 4000) {
	if(is.null(model$lambda_priors)) model <- set_priors(model)
	if(is.null(model$P)) model <- set_parameter_matrix(model)

	model$prior_distribution <- t(replicate(n_draws, draw_lambda(model)))
	message(paste("Prior distribution based on", n_draws, "draws attached to model"))
	return(model)
}




#' Add a true prior distribution
#'
#' Add a vector of true parameters -- can be use to draw data, rather than true vector
#'
#' @param model A model generated by \code{make_model}
#' @param lambda lambda vector to add to model
#' @param random If true, random truth drawn from priors
#' @param average If true, prior mean used
#' @export
#' @examples
#' set_lambda(make_model("X -> Y"), average = TRUE)
#'
set_lambda <- function(model, lambda = NULL, random = FALSE, average = FALSE) {

	if(!is.null(lambda)) message("User supplied lambda attached to model")

	if(is.null(lambda) &!random & !average) stop("Please provide lambda or set random=TRUE or average=TRUE")

	if(random & average) stop("random and average should not both be TRUE")

	if(is.null(lambda)) {

		# Random lambda
		if(random) {
			message(paste("Random parameter generated and attached to model"))
			lambda <- draw_lambda(model)}

		# Average lambda
		if(average) {

			if(average & is.null(model$lambda_prior)) {
				message("Model does not have priors; flat priors assumed")
				model <- set_priors(model)}

			lambda <- model$lambda_priors
			for( j in model$variables){
				x <- startsWith(names(lambda), paste0(j,"."))
				lambda[x] <- lambda[x]/sum(lambda[x])
			}
			message(paste("Prior mean generated and attached to model"))

			lambda
		}
	}

	model$lambda <- lambda
	return(model)
}
