#' Set priors
#'
#' @param pcm A dag created by make_dag()
#' @param prior_distribution A character indicating the prior distribuiton
#' @param alphas the hyperparameters of the dirichlet distribution.
#'
#' @export
#' @examples
#' XYdag <- make_dag(add_edges(parent = "X", children = c("Y")))
#' #  Default sets all priors to 1
#' set_priors(pcm = XYdag)
#' #  Set all priors to 0.5
#' set_priors(pcm = XYdag, prior_distribution = "jeffreys")
#' #  Set all priors to infinity
#' set_priors(pcm = XYdag, prior_distribution = "certainty")
#'
#'#  set all priors to 1 except for prior of nodal_type X0
#' set_priors(pcm = XYdag, alpha = list(X = c(X0 = 2)))
#'#  specify priors for each of the nodal_types in pcm
#' set_priors(pcm = XYdag, prior_distribution = "uniform" ,
#'            alpha = list(X = c(X0 = 2, X1 = 1),  Y = c(Y00 = 1, Y01 = 2, Y10 = 2, Y11 = 1)))
#' # set all priors to 10
#' set_priors(pcm = XYdag, prior_distribution = NULL,
#'            alpha =  10)
set_priors  <- function(pcm,  prior_distribution = "uniform", alphas = NULL){

	dag <- pcm$dag
	nodal_types   <- get_nodal_types(dag)

	n_nodal_types <- length(unlist(nodal_types))

	alphas_vector <- unlist(alphas)
	n_alphas      <- length(alphas_vector)
	alpha_names   <- names(alphas_vector)

	type_names  <- unlist(sapply(1:length(nodal_types), function(i){
		name <- names(nodal_types)[i]
		a <- nodal_types[[i]]
		paste(name, a, sep =".")
	}))

	not_in_dag <- !alpha_names %in% type_names
	if(any(not_in_dag )){
		index <- which(not_in_dag)
		error_text	<- sapply(index, function(i){

			reversed_name <- sapply(lapply(strsplit(alpha_names[i], NULL), rev), paste, collapse = "")
			splitted_name <- unlist(  strsplit(sub("[.]", ";", reversed_name), ";"))

			paste0("\n variable ", splitted_name[2], " and corresponding nodal_type ",  splitted_name[1], " must match variables in dag and nodal_types syntax")

		})
	}
	stop(	error_text )


	if(any(	alphas_vector  <= 0)){
		stop("alphas must be positive real numbers")
	}

	if(!is.null(prior_distribution)){
	if(prior_distribution == "uniform"){

		lambdas <- rep(1, n_nodal_types )

	} else if(prior_distribution == "jeffreys"){

		lambdas <- rep(0.5, n_nodal_types )

	} else if(prior_distribution == "certainty"){

		lambdas <- rep(Inf, n_nodal_types )

	} } else{

		if(n_alphas == 1){

			lambdas <- rep(alphas, n_nodal_types )

		} else if(n_alphas == n_nodal_types){

			lambdas <- alphas

		} else{

			stop("if prior_distribution is not specified, alphas must contain either a value for each nodal_type or a single value to be assigned to all nodal types.")

		}
	}


	names(lambdas) <- type_names

	lambdas[alpha_names] <- alphas_vector

pcm$lambda_priors <- lambdas

# TO DO: define print.dag ?
cat("$lambdas_prior \n")
print(pcm$lambda_priors)
cat("dag: \n")
dag

}
