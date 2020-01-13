
#' Make Priors
#'
#' A flexible function to generate priors for a model.
#'
#' Four arguments govern *which* parameters should be altered. The default is "all" but this can be reduced by specifying
#'
#' * \code{label} The label of a particular nodal type, written either in the form Y0000 or Y.Y0000
#'
#' * \code{node}, which restricts for example to parameters associated with node "X"
#'
#' * \code{statement}, which restricts for example to nodal types that satisfy the statment "Y[X=1] > Y[X=0]"
#'
#' * \code{confound}, which restricts for example to nodal types that satisfy the statment "Y[X=1] > Y[X=0]"
#'
#' Two arguments govern what values to apply:
#'
#' * alphas is one or more non negative numbers and
#'
#' * "distribution" indicates one of a common class: uniform, jeffreys, or "certain"
#'
#' Any arguments entered as lists or vectors of size > 1 should be of the same length as each other.
#'
#' @param model A model created with \code{make_model}
#' @param distribution String (or list of strings) indicating a common prior distribution (uniform, jeffreys or certainty)
#' @param alphas Real positive numbers giving hyperparameters of the Dirichlet distribution
#' @param node A string (or list of strings) indicating nodes for which priors are to be altered
#' @param label String. Label for nodal type indicating nodal types for which priors are to be altered
#' @param statement A causal query (or list of queries) that determines nodal types for which priors are to be altered
#' @param confound A confound named list that restricts nodal types for which priors are to be altered. Adjustments are limited to nodes in the named list.
#' For instance \code{confound = list(X  = Y[X=1]> Y[X=0])} adjust parameters on X that are conditional on nodal types for Y.
#'
#' @family priors
#' @export
#' @examples
#' model <- make_model("X -> M -> Y")
#' make_priors(model, node = "X", alphas = 3)
#' make_priors(model, node = c("X", "Y"), alphas = 3)
#' make_priors(model, node = list("X", "Y"), alphas = list(3, 6))
#' make_priors(model, node = c("X", "Y"), distribution = c("certainty", "jeffreys"))
#' make_priors(model, statement = "Y[M=1] > Y[M=0]", alphas = 3)
#' make_priors(model, statement = c("Y[M=1] > Y[M=0]", "M[X=1]== M[X=0]"), alphas = c(3, 2))
#' \dontrun{
#' # Error if statement seeks to
#' make_priors(model, statement = "Y[X=1] > Y[X=0]", alphas = 3)
#' }
#' model <- make_model("X->Y") %>%
#'  set_confound(list(X = "Y[X=1] > Y[X=0]", X = "Y[X=1] < Y[X=0]"))
#' make_priors(model,
#'             confound = list(X="Y[X=1] > Y[X=0]",
#'                             X="Y[X=1] < Y[X=0]"),
#'             alphas = c(3, 6))
#' make_model("X -> Y") %>%
#'   set_confound(list(X = "Y[X=1]>Y[X=0]"))%>%
#'   make_priors(statement = "X==1",
#'               confound = list(X = "Y[X=1]>Y[X=0]", X = "Y[X=1]<Y[X=0]"),
#'               alphas = c(2, .5))

make_priors <- function(model,
												distribution=NA,
												alphas=NA,
												node = NA,
												label=NA,
												statement=NA,
												confound=NA){

	# Housekeeping regarding argument lengths
	args <- list(distribution = distribution, alphas = alphas, node = node,
							 label = label, statement = statement, confound = confound)
	arg_provided <- unlist(lapply(args, function(x) any(!is.na(x))))
	arg_length   <- unlist(lapply(args, length))

	# Easy case: If all but node, label, or alphas are of length 1 then simply apply make_priors_single
	for(j in c("node", "label", "alphas")) {
		if(max(arg_length[names(args)!=j])==1) return(
			gbiqq:::make_priors_single(model, distribution=distribution, alphas=alphas,
															node = node, label=label, statement=statement,
															confound=confound)
		)}

	# Harder case: Otherwise all arguments turned to lists and looped through
	# updating priors each time

	if(sum(arg_length>1)>1) {if(sd(arg_length[arg_length>1])>0)
		stop("Provided arguments of length >1 should be of the same length") }

	# Function uses mapply to generate task_list
	f <- function(distribution, alphas, node, label, statement, confound, confound_names){

		# Non NA confounds need to be in a named list
		if(!is.na(confound)) {
			confound <- list(confound)
		  names(confound) <- confound_names}

		list(distribution = distribution, alphas = alphas,
				 node = node, label = label, statement = statement, confound = confound)
		}
	if(is.null(names(confound))) names(confound) <- NA

	task_list <- mapply(f, distribution = distribution, alphas = alphas,
											node = node, label = label, statement = statement, confound = confound,
											confound_names = names(confound))


	for(i in 1:ncol(task_list)) {
		arguments <- task_list[,i]
			model$parameters_df$priors <-
					gbiqq:::make_priors_single(model,
														 distribution=arguments$distribution,
														 alphas=arguments$alphas,
														 node = arguments$node,
														 label=arguments$label,
														 statement=arguments$statement,
														 confound=arguments$confound)}
	get_priors(model)
	}


#' make_priors_single
#'
#' This is the one step function for make_priors, it creates the priors to be passed on nodal types with \code{set_priors}. See \code{make_priors} for more help.
#'
#' Forbidden statements include:
#' \itemize{
#'   \item Setting \code{distribution} and \code{alphas} at the same time.
#'   \item Setting a \code{distribution} other than uniform, jeffreys or certainty.
#'   \item Setting negative priors.
#' }
#'
#'
#' @param model A model created with \code{make_model}
#' @param distribution String indicating a common prior distribution (uniform, jeffreys or certainty)
#' @param alphas Real positive numbers giving hyperparameters of the Dirichlet distribution
#' @param node A string indicating nodes for which priors are to be altered
#' @param label String. Label for nodal type indicating nodal types for which priors are to be altered
#' @param statement A causal query that determines nodal types for which priors are to be altered
#' @param confound A confound statement that restricts nodal types for which priors are to be altered
#'
#' @family priors
#' @examples
#' model <- make_model("X -> M -> Y; X->Y")
#'
#' gbiqq:::make_priors_single(model, distribution = "jeffreys")
#'
#' gbiqq:::make_priors_single(model, alphas = 3)
#'
#' # Examples of selecting subsets
#' # By node
#' gbiqq:::make_priors_single(model, node = "M", alphas = 8)
#'
#' # By nodal type statement
#' gbiqq:::make_priors_single(model,
#'         statement = "(Y[X=1, M = .] > Y[X=0, M = .])", alphas = 2)
#'
#' # By nodal type label (safest to provide node also)
#' gbiqq:::make_priors_single(model, node = "X", label = "0", alphas = 9)
#'
#' # By confound query: Applies only to types that are involved in confounding
#' # Only alters named node in confound, even if other nodes are listed in "nodes"
#' confounds <- list(X = "Y[X=1] > Y[X=0]", X = "Y[X=1] < Y[X=0]")
#' model     <- make_model("X->Y") %>% set_confound(confounds)
#' gbiqq:::make_priors_single(model, confound = confounds[1], alphas = 3)
#' gbiqq:::make_priors_single(model, node = "Y", confound = confounds[1], alphas = 3)
#'
#' # A residual  confound condition can also be defined
#' gbiqq:::make_priors_single(model, confound = list(X = "!(Y[X=1] > Y[X=0])"), alphas = 3)
#' gbiqq:::make_priors_single(model, confound = list(X = "(Y[X=1] == Y[X=0])"), alphas = 3)
#'
#' # make_priors_single can also be used for some vector valued statements
#' model <- make_model("X -> M -> Y")
#' gbiqq:::make_priors_single(model, node = c("X", "Y"), alphas = 2)
#' gbiqq:::make_priors_single(model, label = c("1", "01"), alphas = 2)
#'
#' # Incompatible conditions produce no change
#' # Such cases best handled by  make_priors
#' gbiqq:::make_priors_single(model, node = "X", label = "01", alphas = 2)
#'
#' # Problematic example
#' \dontrun{
#' gbiqq:::make_priors_single(model, alphas = 1:2)
#' }
#'


make_priors_single <- function(model,
												distribution = NA,
												alphas = NA,
												node = NA,
												label = NA,
												statement = NA,
												confound = NA){

	#1. House keeping

	# 1. Data from model
	priors      <- get_priors(model)
	prior_names <- names(priors)                 #
	params      <- model$parameters_df$param_names     # Parameter names
	to_alter    <- rep(TRUE, length(priors))   # Subset to alter: Starts at full but can get reduced

	# 1.2. If neither a distribution or alphas vector provided then return existing priors
	if (all(is.na(distribution)) & all(is.na(alphas))){
		warning("neither distribution nor alphas provided; no change to priors")
		return(priors)}

	# 1.3. No prior distribution and alphas at the same time
	if (!all(is.na(distribution)) & !all(is.na(alphas))){
		warning("alphas and distribution cannot be declared at the same time. try sequentially; no change to priors")
		return(priors)}

	#1.4. Alphas negatives
	if (!all(is.na(alphas))) {if(min(alphas) < 0) stop("alphas must be non-negative")}

	# 1.5 distribution should be a scalar
	if(max(length(distribution), length(confound), length(statement))>1)
		stop("distribution, statement, and confound should be scalars in make_prior_single")

	# 1.6 confound is a named list and if provided only the named node is changed
	if(!is.na(confound) & any(is.na(node))) node[is.na(node)] <- names(confound)

  # A. Where to make changes?
	#########################################################################

	# A1 Do not alter if node is not in listed nodes
	if(!all(is.na(node))){
		if(!all(node %in% model$nodes))
			stop("listed nodes must be nodes in the model")
		to_alter[!(model$parameters_df$node %in% node)] <- FALSE}

	# A2 Do not alter if nodal type is not part of a given statement
		if (!all(is.na(statement))) {
			lnt     <- lookup_nodal_type(model, statement)
			l_types <- lnt$types

			to_alter[!(model$parameters_df$node %in% lnt$node &
								 model$parameters_df$nodal_type %in% names(l_types[l_types]))] <- FALSE
			}

	# A3 Do not alter if nodal type is not one of listed nodal types
		if(!all(is.na(label))){
  	to_alter[!(model$parameters_df$nodal_type %in% label) ] <- FALSE
		}

	# A4 Do not alter if confound condition is not met:
	# For instance if a condition is "Y[X=1]>Y[X=0]" then any parameter that does *not*
	# contribute to a causal type satisfying this condition is not modified
	if(!all(is.na(confound))){
		P_short <- model$P[, get_query_types(model, confound[[1]])$types]
		to_alter[(apply(P_short, 1, sum) == 0)] <- FALSE
	}

  # B. What values to provide?
	#########################################################################
	# Provide alphas unless a distribution is provided
	if(!is.na(distribution)) {
		if(!(distribution %in% c("uniform", "jeffreys", "certainty")))
			stop("distribution should be either 'uniform', 'jeffreys', or 'certainty'.")
		alphas <- switch (distribution, "uniform" = 1, "jeffreys" = .5, "certainty" = 10000)}


	# C MAGIC
	#########################################################################

	if(sum(to_alter)==0) {message("No change to priors"); return(priors)}

  if((length(alphas) != 1) & (length(alphas) != sum(to_alter)))
  	stop(paste("Trying to replace ", length(to_alter), " parameters with ", length(alphas), "values"))

	priors[to_alter] <- alphas

	return(priors)
	}


#' Set prior distribution
#'
#' A flexible function to add priors to a model.
#'
#' Four arguments govern *which* parameters should be altered. The default is "all" but this can be reduced by specifying
#'
#' * \code{label} The label of a particular nodal type, written either in the form Y0000 or Y.Y0000
#'
#' * \code{node}, which restricts for example to parameters associated with node "X"
#'
#' * \code{statement}, which restricts for example to nodal types that satisfy the statment "Y[X=1] > Y[X=0]"
#'
#' * \code{confound}, which restricts for example to nodal types that satisfy the statment "Y[X=1] > Y[X=0]"
#'
#' Two arguments govern what values to apply:
#'
#' * alphas is one or more non negative numbers and
#'
#' * "distribution" indicates one of a common class: uniform, jeffreys, or "certain"
#'
#' Any arguments entered as lists or vectors of size > 1 should be of the same length as each other.
#'
#' For more examples and details see \code{make_priors}
#'
#' @param model A model created with \code{make_model}
#' @param priors A optional vector of positive reals indicating priors over all parameters. These are interepreted as arguments for Dirichlet distributions---one for each parameter set. To see the structure of parameter sets examine model$parameters_df
#' @param distribution String (or list of strings) indicating a common prior distribution (uniform, jeffreys or certainty)
#' @param alphas Real positive numbers giving hyperparameters of the Dirichlet distribution
#' @param node A string (or list of strings) indicating nodes for which priors are to be altered
#' @param label String. Label for nodal type indicating nodal types for which priors are to be altered
#' @param statement A causal query (or list of queries) that determines nodal types for which priors are to be altered
#' @param confound A confound statement (or list of statements) that restricts nodal types for which priors are to be altered
#' @export
#' @family priors
#' @examples
#'
#' library(dplyr)
#' model <- make_model("X -> Y") %>%
#'   set_priors(alphas = 3)
#' get_priors(model)
#'
#'model <- make_model("X -> Y") %>%
#' set_priors(distribution = "jeffreys")
#' get_priors(model)
#'
#'model <- make_model("X -> Y") %>%
#' set_priors(1:6)
#' get_priors(model)
#' model <- make_model("X -> Y") %>%
#' set_priors(node = "Y", alphas = 2)
#' get_priors(model)


set_priors  <- function(model,
													priors = NULL,
													distribution=NA,
													alphas=NA,
													node = NA,
												  label=NA,
													statement=NA,
													confound=NA) {

	if(is.null(priors)) priors <- make_priors(model,
			                            					distribution = distribution,
																						alphas = alphas,
																						node = node,
																						label = label,
																						statement = statement,
																						confound = confound)

	 if(!is.null(priors) & is.character(priors)) stop("Argument priors must be a vector of positive real number")

   model$parameters_df$priors  <- priors

   model

}



#' Get priors
#'
#' Extracts priors as a named vector
#'
#' @param model A model object generated by make_model().
#'
#' @export
#' @family priors
#' @examples
#' get_priors(make_model("X -> Y"))

get_priors  <- function(model) {

	x <- model$parameters_df$priors
	names(x) <-  model$parameters_df$param_names
	x

}




