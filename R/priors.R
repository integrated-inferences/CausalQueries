
#' Make Priors
#'
#' A flexible function to add priors to a model.
#'
#' Four arguments govern *which* parameters should be altered. The default is "all" but this can be reduced by specifying
#' * \code{label} The label of a particular nodal type, written either in the form Y0000 or Y.Y000
#' * \code{node}, which restricts for example to parameters associated with node "X"
#' * \code{statement}, which restricts for example to nodal types that satisfy the statment "Y[X=1] > Y[X=0]"
#' * \code{confound}, which restricts for example to nodal types that satisfy the statment "Y[X=1] > Y[X=0]"
#'
#' Two arguments govern what values to apply: alphas is one or more non negative numbers and "prior" distribution indicates one of a common class: uniform, jeffreys, or "certain"
#'
#' @param model A model created with \code{make_model}
#' @param prior_distribution A common prior distribution (uniform, jeffreys or certainty)
#' @param alphas Hyperparameters of the Dirichlet distribution
#' @param node A node
#' @param statement A query
#' @param confound A confound statement
#'
#' @export
#' @examples
#' model <- make_model("X -> Y")
#' make_priors(model, node = list("X", "Y"), alphas = list(3, 6))
#' model <- make_model("X->Y")
#' model <- set_confound(model, list(X = "Y[X=1] > Y[X=0]"))
#' model <- set_confound(model, list(X = "Y[X=1] < Y[X=0]"))
#' make_priors(model, confound = list("Y[X=1] > Y[X=0]", "Y[X=1] < Y[X=0]"), alphas = list(3, 6))

make_priors <- function(model=list(model),
												prior_distribution=NA,
												alphas=NULL,
												node = NA,
												statement=NA,
												confound=NA,
												label=NA){

	args <- list(prior_distribution, alphas, node, statement, confound, label)

	arg_provided <- lapply(args, function(x) !is.na(x))
	arg_length   <- lapply(args, length)

	if(sum(arg_provided)>1) {if(sd(unlist(arg_length[arg_provided]))>0)
		stop("Provided arguments should be of the same length") }

	for(i in 1:max(unlist(arg_length))) {
			model$parameters_df$priors <-
					make_priors_single(model,
		                    		 node = node[i],
														 prior_distribution=prior_distribution[i],
														 statement=statement[i],
														 confound=confound[i],
														 alphas=alphas[i],
														 label=label[i])}
		}


#' make_priors_single
#'
#' This is the one step function for make_priors, it creates the priors to be passed on nodal types with \code{set_priors}. See \code{make_priors} for more help.
#'
#' Forbidden statements include:
#' \itemize{
#'   \item Setting \code{prior_distribution} and \code{alphas} at the same time.
#'   \item Setting a \code{prior_distribution} other than uniform, jeffreys or certainty.
#'   \item Setting negative priors.
#' }
#'
#'
#' @param model A mode created with \code{make_model}
#' @param prior_distribution A common prior distribution (uniform, jeffreys or certainty)
#' @param alphas Hyperparameters of the Dirichlet distribution
#' @param node A node
#' @param statement A query
#' @param confound A confound statement
#' @param label The name of a nodal type
#'
#' @export
#' @examples
#' model <- make_model("X -> M -> Y; X->Y")
#'
#' make_priors_single(model, prior_distribution = "jeffreys")
#'
#' make_priors_single(model, alphas = 3)
#'
#' # Examples of selecting subsets
#' # By node
#' make_priors_single(model, node = "M", alphas = 8)
#'
#' # By nodal type statement
#' make_priors_single(model, statement = "(Y[X=1, M = .] > Y[X=0, M = .])", alphas = 2)
#'
#' # By nodal type label
#' make_priors_single(model, label = "X0", alphas = 9)
#'
#' # By confound query: Applies only to types that are sometimes involved on confounding
#' # Safest to apply together with node to pick out specific sets
#' model <- make_model("X->Y") %>% set_confound(list(X = "Y[X=1] > Y[X=0]", X = "Y[X=1] < Y[X=0]"))
#' make_priors_single(model, confound = "Y[X=1] > Y[X=0]", alphas = 3)
#' make_priors_single(model, node = "X", confound = "Y[X=1] > Y[X=0]", alphas = 3)
#'
#' # make_priors_single can also be used for some vector valued statements
#' model <- make_model("X -> M -> Y")
#' make_priors_single(model, node = c("X", "Y"), alphas = 2)
#' make_priors_single(model, label = c("X1", "Y01"), alphas = 2)
#'
#' # Incompatible conditions produce no change
#' # Such cases best handled by  make_priors
#' make_priors_single(model, node = "X", label = "Y01", alphas = 2)
#'
#' # Problematic cases
#' \dontrun{
#' make_priors_single(model, alphas = 1:2)
#' }
#'


make_priors_single <- function(model,
												prior_distribution = NA,
												alphas = NULL,
												node = NA,
												statement = NA,
												confound = NA,
												label = NA){

	#1. House keeping

	# 1. Data from model
	priors      <- get_priors(model)
	prior_names <- names(priors)                 #
	params      <- model$parameters_df$param     # Parameter names
	to_alter    <- rep(TRUE, length(priors))   # Subset to alter: Starts at full but can get reduced

	# 1.2. If neither a distribution or alphas vector provided then return existing priors
	if (is.na(prior_distribution) & is.null(alphas)){
		warning("neither prior_distribution nor alphas provided; no change to priors")
		return(priors)}

	# 1.3. No prior distribution and alphas at the same time
	if (!is.na(prior_distribution) & !is.null(alphas)){
		warning("alphas and prior_distribution cannot be declared at the same time. try sequentially; no change to priors")
		return(priors)}

	#1.4. Alphas negatives
	if (!is.null(alphas)) {if(min(alphas) < 0) stop("alphas must be non-negative")}

	# 1.5 prior_distribution should be a scalar
	if(max(length(prior_distribution), length(confound), length(statement))>1)
		stop("prior_distribution, statement, and confound should be scalars in make_prior_single")

  # A. Where to make changes?
	#########################################################################

	# A1 Do not alter if node is not in listed nodes
	if(!all(is.na(node))){
		if(!all(node %in% model$variables))
			stop("listed nodes must be variables in the model")
		to_alter[!(model$parameters_df$param_family %in% node)] <- FALSE}

	# A2 Do not alter if nodal type is not part of a given statement
		if (!all(is.na(statement))) {
			l_types  <- lookup_type(model, statement)$types
			types_to_alter <- names(l_types[l_types])
			to_alter[!(params %in% types_to_alter)] <- FALSE
			}

	# A3 Do not alter if nodal type is not one of listed nodal types
		if(!all(is.na(label))){
  	to_alter[!((params %in% label) | (prior_names %in% label))] <- FALSE
		}

	# A4 Do not alter if confound condition is not met:
	# For instance if a condition is "Y[X=1]>Y[X=0]" then any parameter that does *not*
	# contribute to a causal type satisfying this condition is not modified
	if(!all(is.na(confound))){
		P_short <- model$P[, get_types(model, confound)$types]
		to_alter[(apply(P_short, 1, sum) == 0)] <- FALSE
	}

  # B. What values to provide?
	#########################################################################
	# Provide alphas unless a distribution is provided
	if(!is.na(prior_distribution)) {
		if(!(prior_distribution %in% c("uniform", "jeffreys", "certainty")))
			stop("prior_distribution should be either 'uniform', 'jeffreys', or 'certainty'.")
		alphas <- switch (prior_distribution, "uniform" = 1, "jeffreys" = .5, "certainty" = 10000)}


	# C MAGIC
	#########################################################################

	if(sum(to_alter)==0) {message("No change to priors"); return(priors)}

  if((length(alphas) != 1) & (length(alphas) != sum(to_alter)))
  	stop(paste("Trying to replace ", length(to_alter), " parameters with ", length(alphas), "values"))

	priors[to_alter] <- alphas

	return(priors)
	}





make_alphas <- function(model, alphas = NULL ){

	#	if(is.null(model$P)) 	{model  <- set_parameter_matrix(model)}
	#	P                  <- model$P
	return_alphas      <- alphas
	par_names          <- get_parameter_names(model, include_paramset = FALSE)
	alpha_names        <- names(unlist(alphas))
	# alpha_names        <- 	unlist(lapply(alphas, names))
	pars_in_alpha      <- alpha_names %in% par_names

	# if alpha is defined as vector of queries alphas <- c(`(Y[X=1] == Y[X=0])`  = 3,  `X == 1` = 3  )
	if(is.numeric(alphas) & !is.null(alpha_names)){
		return_alphas  <-   query_to_parameters(model,	 alphas)
	}


	# if alpha contain other than parameter names e.g alphas = list(X = c(X0 = 2, `X == 1` = 3))
	if(is.list(alphas)  & any(! pars_in_alpha)){
		# Prep and translate alpha
		i_queries <- which(!pars_in_alpha)
		alpha_query <- unlist(alphas)[i_queries]
		queries <- names(alpha_query)
		names(alpha_query) <- 	sapply(queries, function(q){
			stop <- gregexpr("\\.", q, perl = TRUE)[[1]][1]
			substr(q, stop + 1, nchar(q))
		})

		translated_alphas  <-  gbiqq:::query_to_parameters(model, alpha_query)

		# Lines below check for discrepancies
		# ok alphas(Y = c(Y00 = 1, `(Y[X=1] == Y[X=0])` = 1)
		# error  alphas(Y = c(Y00 = 2, `(Y[X=1] == Y[X=0])` = 1) --two arguments pointing at the same parameter
		error_message <- gbiqq:::any_discrepancies(alphas, translated_alphas)
		if(!is.null(error_message))	{
			stop("\n Please solve the following discrepancies \n", paste(error_message))
		}
		# Get alphas that were specificied as par_names as opposed to queries
		alpha_param <- sapply(alphas, function(a){
			#	a[names(a) %in% rownames(P)]

			a[names(a) %in% par_names]

		}, simplify = FALSE)

		# combine translated alphas and alpha_parameters by parameter set
		return_alphas <- gbiqq:::combine_lists(list1 = alpha_param, list2 =translated_alphas)
	}
	return_alphas
}



#' Set prior distribution
#'
#' Set prior distribution. Use \code{make_alphas} to use causal types to indicate priors.
#' @param model A model object generated by \code{make_model()}.
#' @param priors A numeric vector. Dirichlet hyperparameters.
#' @param prior_distribution A character string. Indicates the prior distribution to be used. Can be 'uniform', 'jeffreys', or 'certainty'.
#' @param alphas A numeric vector. Hyperparameters of the Dirichlet distribution to be passed to \code{make_priors()}.
#' @export
#' @examples
#'
#' library(dplyr)
#' model <- make_model("X -> Y") %>%
#'   set_priors(
#'            alphas = list(
#'              X = c(`X == 1` = 3),
#'              Y = c(`(Y[X=1] != Y[X=0])` = 3)))
#' get_priors(model)
#'
#'model <- make_model("X -> Y") %>%
#' set_priors(prior_distribution = "jeffreys")
#' get_priors(model)
#'
#'model <- make_model("X -> Y") %>%
#' set_priors(1:6)
#' get_priors(model)

set_priors  <- function(model,
												priors = NULL,
												prior_distribution = "uniform",
												alphas = NULL) {

	if(is.null(priors)) priors <- make_priors(model,
			                            					prior_distribution = prior_distribution,
																						alphas = alphas)

   model$parameters_df$priors  <- priors

   model

}


#' Add prior distribution draws
#'
#' Add n_param x n_draws database of possible lambda draws to the model.
#'
#' @param model A model object generated by make_model().
#' @param n_draws A scalar. Number of draws.
#' @export
#' @examples
#' model <- make_model("X -> Y") %>% set_prior_distribution(5)
#' model$prior_distribution
#'
set_prior_distribution <- function(model, n_draws = 4000) {

  if(is.null(model$P)) model <- set_parameter_matrix(model)

  model$prior_distribution <- t(replicate(n_draws, draw_parameters(model)))

  model
}



#' Get priors
#'
#' Extracts priors as a namesd vector
#'
#' @param model A model object generated by make_model().
#'
#' @export
#' @examples
#' get_priors(make_model("X -> Y"))

get_priors  <- function(model) {

	x <- model$parameters_df$priors
	names(x) <-  model$parameters_df$param_names
	x

}


