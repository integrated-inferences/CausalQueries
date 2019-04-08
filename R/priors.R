#' Define priors
#'
#' @param dag A dag created by make_dag()
#' @param nodal_types
#'
#' @export
#'
define_priors  <- function(dag = NULL,  prior_distribution = "uniform", alphas = NULL){

	nodal_types   <- get_nodal_types(dag)
	n_nodal_types <- length(nodal_types)
	type_names    <- names(nodal_types)
	n_alphas      <- length(alphas)
  if(any(alphas <= 0)){
  	stop("alphas must be real positive numbers")
  }


	if(prior_distribution == "uniform"){
		lambdas <- rep(1, n_nodal_types )

	}
	else if(prior_distribution == "jeffreys"){
		lambdas <- rep(0.5, n_nodal_types )
	}
	else if(is.null(prior_distribution)){
		if(n_alphas == 1){
			if(alphas < 0)
				stop(alpha)


		} else{
			stop("prior_distribution or alphas must be specified")


		}

	}







}
