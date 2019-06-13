#' Set priors
#'
#' @param model A model created by make_model()
#' @param prior_distribution A character indicating the prior distribuiton
#' @param alphas the hyperparameters of the dirichlet distribution.
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
#'            alphas = list(X = c(X0 = 2, `X == 1` = 3),  Y = c(Y00 = 1, `(Y[X=1] > Y[X=0])` = 3, Y01 = 2, `(Y[X=1] == Y[X=0])`  = 3)))
#' # set all priors to 10
#' make_priors(model = XYmodel, alphas =  10)
#'
make_priors_temp <- function(model,  prior_distribution = "uniform", alphas = NULL ){

	P                  <- get_parameter_matrix(model)
	n_params           <- nrow(P)
	param_set          <- attr(P, "param_set")
	param_sets         <- unique(param_set)
	n_param_sets       <- length(param_sets)
	par_names          <- paste0(param_set, ".", rownames(P))

	alpha_names <- names(unlist(alphas))
	in_par_names <- alpha_names %in% par_names

  translate_expression <- function(model, a){
  	if(length(a) == 1){
  		translated_a <-  gbiqq:::types_to_nodes(model, names(a))
  		names(translated_a[[1]])[1] <- translated_a[[1]][1]
  		translated_a[[1]][1] <- as.numeric(a)
  	} else{
		nodal_types <- gbiqq:::types_to_nodes(model, names(a))
		translated_a <- 	sapply(names(nodal_types), function(v){
			v_nodal_types <- nodal_types[[v]]
			out <- unlist(sapply(1:length(v_nodal_types), function(j){
				query <- names(v_nodal_types)[j]
				nt <- v_nodal_types[[j]]
				value <- a[query]
				translated_a <- rep(value, length(nt))
				names(translated_a) <- nt
				translated_a
			}))


		}, simplify = FALSE)
		attr(translated_a, "query") <- 	nodal_types
		}
		return(translated_a)
  }



  if(is.numeric(alphas) & !is.null(alpha_names)){
  	alphas <-   translate_expression(model, alphas)
  }


 if(is.list(alphas)  & any(! in_par_names )){
 	i_queries <- which(!in_par_names)
 	a <- unlist(alphas)[i_queries]
 	queries <- names(a)
  names(a) <- 	sapply(queries, function(q){
  	stop <- gregexpr("\\.", q, perl = TRUE)[[1]][1]
  	substr(q, stop + 1, nchar(q))
  })

 	translated_alphas  <-  translate_expression(model, a)

 	repeated_parameters <- names(unlist( 	translated_alphas  )) %in% names(unlist(alphas))
 	if(any(repeated_parameters)){
   query_alpha <- attr(translated_alphas, "query")
   i_repeated  <-	which(repeated_parameters)
   q_repeated  <- unlist(translated_alphas)[i_repeated]
   names(q_repeated) <- names(unlist(translated_alphas))[i_repeated]
   names(q_repeated) <- sapply(  names(q_repeated) , function(q){
   	stop <- gregexpr("\\.", q, perl = TRUE)[[1]][1]
   	substr(q, stop + 1, nchar(q))
   })
   q_repeated <- q_repeated[order(names(q_repeated))]

   i_alphas_repeated <- names(unlist(alphas)) %in% names(unlist(translated_alphas))
   alphas_repeated <-  unlist(alphas)[i_alphas_repeated]
   names(alphas_repeated)  <-  names(unlist(alphas))[i_alphas_repeated]

   names(alphas_repeated) <- sapply(  names(alphas_repeated) , function(q){
   	stop <- gregexpr("\\.", q, perl = TRUE)[[1]][1]
   	substr(q, stop + 1, nchar(q))
   })
   alphas_repeated <-  alphas_repeated[order(names(alphas_repeated))]

   if(!identical(alphas_repeated, q_repeated)){
   a_discrepancies  <- 	alphas_repeated[alphas_repeated != q_repeated]
   q_discrepancies  <- 	q_repeated[alphas_repeated != q_repeated]
   nnn <- names(alphas_repeated)[alphas_repeated != q_repeated]

   qqq <- names(unlist(query_alpha))[q_repeated]
   error_message <- unlist(sapply(query_alpha, function(q){
  			sapply(1:length(q), function(j){
   		r <- q[[j]] %in%  nnn
   	   if(any(r)){
       paste0( names(q)[j] , " = ", q_discrepancies[ q[[j]][r]] ,", ", q[[j]][r], " = ", a_discrepancies[ q[[j]][r]], "\n")
   	   } })}))


   stop("\n Please solve the following discrepancies \n", paste(error_message))


   }


 	}


 }



}
#'
#'
make_priors  <- function(model,    prior_distribution = "uniform", alphas = NULL ){

	if(!is.null(prior_distribution)){
		if(!(prior_distribution %in% c("uniform", "jeffreys", "certainty")))
			stop("prior_distribution should be either 'uniform', 'jeffreys', or 'certainty'.")
	}

	P                  <- get_parameter_matrix(model)
	n_params           <- nrow(P)
	param_set          <- attr(P, "param_set")
	param_sets         <- unique(param_set)
	n_param_sets       <- length(param_sets)
	par_names          <- paste0(param_set, ".", rownames(P))


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

	# Calculate priors
	if(!is.null(prior_distribution)){
		if(prior_distribution == "uniform")   priors <- rep(1, n_params )
		if(prior_distribution == "jeffreys")  priors <- rep(0.5, n_params )
		if(prior_distribution == "certainty") priors <- rep(10000, n_params )

	} else{
		if(n_alphas == 1)                     priors <- rep(alphas, n_params)
		if(n_alphas == n_params)              priors <- alphas}

	# Substitute alpha vector when provided
	priors[alpha_names] <- alphas_vector

	# result
	names(priors)       <- par_names
	priors

}



#' Set prior distribution
#'
#' @param model A model created by make_model()
#' @param priors Existing vector of Dirichlet hyperparameters
#' @param prior_distribution A character indicating the prior distribuiton
#' @param alphas Vector of hyperparameters of the dirichlet distribution to be passed to \code{make_priors}.
#'
#' @export

set_priors  <- function(model,
												priors = NULL,
												prior_distribution = "uniform",
												alphas = NULL) {

	if(is.null(priors)) priors <- make_priors(model,
			                            					prior_distribution = prior_distribution,
																						alphas = alphas)

   model$priors  <- priors
#   message(paste("Priors attached to model"))
   model

}



#' Get prior distribution
#'
#' @param model A model created by make_model()
#' @param priors Vector of Dirichlet hyperparameters
#' @param prior_distribution A character indicating the prior distribuiton
#' @param alphas the hyperparameters of the dirichlet distribution.
#'
#' @export

get_priors  <- function(model,  priors = NULL, prior_distribution = "uniform", alphas = NULL) {

	if(!is.null(model$priors)) return(model$priors)

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

  if(is.null(model$priors)) model <- set_priors(model)
  if(is.null(model$P)) model <- set_parameter_matrix(model)

  model$prior_distribution <- t(replicate(n_draws, draw_lambda(model)))
#  message(paste("Prior distribution based on", n_draws, "draws attached to model"))

	return(model)
}







#' Add a true prior distribution
#'
#' Add a vector of true parameters -- can be use to draw data, rather than true vector
#'
#' @param model A model generated by \code{make_model}
#' @param parameters parameter vector to add to model
#' @param type An element of ("flat", "prior_mean", "posterior_mean", "prior_draw", "posterior_draw"). Indicates type of parameters to set.
#' @export
#' @examples
#' model <- set_parameters(make_model("X -> Y"), type = "flat")
#'
set_parameters <- function(model,
													 parameters = NULL,
													 type = NULL) {

	if (!is.null(parameters)) {
		model$parameters <- parameters
		return(model)}

	# Flat lambda
	if (type == "flat")  {
		parameters <- make_priors(model, prior_distribution = "uniform")
		for (j in model$variables) {
			x <- startsWith(names(parameters), paste0(j, "."))
			parameters[x] <- parameters[x] / sum(parameters[x])}}

	if (type == "prior_mean") {
		if (is.null(model$prior)) stop("Prior distribution required")

		parameters <- model$priors

		for (j in model$variables) {
			x <- startsWith(names(parameters), paste0(j, "."))
			parameters[x] <- parameters[x] / sum(parameters[x])}}

		if (type == "prior_draw") {
			if (is.null(model$prior)) stop("Prior distribution required")

			parameters <- draw_lambda(model)}

		if (type == "posterior_mean") {
			if (is.null(model$posterior)) stop("Posterior distribution required")
			parameters <-
				apply(rstan::extract(model$posterior, pars = "lambdas")$lambdas,
							2, mean)}

		if (type == "posterior_draw") {
			if (is.null(model$posterior))
				stop("Posterior distribution required")
			df <- rstan::extract(model$posterior, pars = "lambdas")$lambdas
			parameters <- df[sample(nrow(df), 2),]}

		model$parameters <- parameters

		return(model)
}
