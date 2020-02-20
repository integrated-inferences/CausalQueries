#' Make values task list
#'
#' A function to generate a list of parameter arguments.
#'
#'
#' @param distribution String (or list of strings) indicating a common prior distribution (uniform, jeffreys or certainty)
#' @param x Real positive numbers. For priors these are hyperparameters of the Dirichlet distribution. For parameters these are probabilities.
#' @param node A string (or list of strings) indicating nodes for which priors are to be altered
#' @param label String. Label for nodal type indicating nodal types for which priors are to be altered
#' @param statement A causal query (or list of queries) that determines nodal types for which priors are to be altered
#' @param confound A confound named list that restricts nodal types for which priors are to be altered. Adjustments are limited to nodes in the named list.
#' @param nodal_type String. Label for nodal type indicating nodal types for which priors are to be altered
#' @param param_set String. Indicates the name of the set of parameters to be modified (useful when setting confounds)
#' @param param_names String. The name of specific parameter in the form of, for example, 'X.1', 'Y.01'
#'
#'
#' For instance \code{confound = list(X  = Y[X=1]> Y[X=0])} adjust parameters on X that are conditional on nodal types for Y.
#'
#' @family priors
#' @examples
#' gbiqq:::make_values_task_list(node = 'X', x = 3)
#' gbiqq:::make_values_task_list(node = c('X', 'Y'), x = 2:3)
#' gbiqq:::make_values_task_list(node = c('X', 'Y'), x = list(1, 2:4))


make_values_task_list <- function(distribution = NA, x = NA, node = NA, label = NA, statement = NA,
																	confound = NA, nodal_type = NA, param_names = NA, param_set = NA) {

	# Housekeeping regarding argument lengths
	args <- list(distribution = distribution, x = x, node = node, label = label, statement = statement,
							 confound = confound, nodal_type = nodal_type, param_names = param_names, param_set = param_set)
	arg_length   <- unlist(lapply(args, length))

	# All arguments turned to lists and looped through updating priors each time

	if (sum(arg_length > 1) > 1) {
		if (sd(arg_length[arg_length > 1]) > 0)
			stop("Provided arguments of length >1 should be of the same length")
	}

	# Function uses mapply to generate task_list
	f <- function(distribution, x, node, label, statement, confound, confound_names, nodal_type,
								param_names, param_set) {

		# Non NA confounds need to be in a named list
		if (!is.na(confound)) {
			confound <- list(confound)
			names(confound) <- confound_names
		}

		list(distribution = distribution, x = x, node = node, label = label, statement = statement,
				 confound = confound, nodal_type = nodal_type, param_names = param_names, param_set = param_set)
	}
	if (is.null(names(confound)))
		names(confound) <- NA

	mapply(f, distribution = distribution, x = x, node = node, label = label,
				 statement = statement, confound = confound, confound_names = names(confound), nodal_type = nodal_type,
				 param_names = param_names, param_set = param_set)
}


#' make_par_values
#'
#' This is the one step function for make_priors and make_parameters.
#' See \code{make_priors} for more help.
#'
#' Forbidden statements include:
#' \itemize{
#'   \item Setting \code{distribution} and \code{values} at the same time.
#'   \item Setting a \code{distribution} other than uniform, jeffreys or certainty.
#'   \item Setting negative values.
#' }
#'
#' @param model A model created with \code{make_model}
#' @param y Vector of real non negative values to be changed
#' @param x Vector of real non negative values to be substituted into y
#' @param distribution String indicating a common prior distribution (uniform, jeffreys or certainty)
#' @param node A string indicating nodes for which priors are to be altered
#' @param label String. Label for nodal type indicating nodal types for which priors are to be altered
#' @param statement A causal query that determines nodal types for which priors are to be altered
#' @param confound A confound statement that restricts nodal types for which priors are to be altered
#' @param nodal_type String. Label for nodal type indicating nodal types for which priors are to be altered
#' @param param_set String. Indicates the name of the set of parameters to be modified (useful when setting confounds)
#' @param param_names String. The name of specific parameter in the form of, for example, 'X.1', 'Y.01'
#' @param normalize Logical. If TRUE normalizes such that param set probabilities sum to 1.
#'
#' @family priors
#' @examples
#' model <- make_model('X -> M -> Y; X -> Y')
#'
#' gbiqq:::make_par_values(model, distribution = 'jeffreys')
#'
#' gbiqq:::make_par_values(model, x = 3)
#'
#' # Selecting subsets:
#'
#' # By node
#' gbiqq:::make_par_values(model, node = 'M', x = 8)
#'
#' # By nodal type statement
#' gbiqq:::make_par_values(model,
#'         statement = '(Y[X=1, M = .] > Y[X=0, M = .])', x = 2)
#'
#' # By nodal type label (safest to provide node also)
#' gbiqq:::make_par_values(model, node = 'X', label = '0', x = 9)
#'
#' # By confound query: Applies only to types that are involved in confounding
#' # Only alters named node in confound, even if other nodes are listed in 'nodes'
#' confounds <- list(X = 'Y[X=1] > Y[X=0]', X = 'Y[X=1] < Y[X=0]')
#' model     <- make_model('X->Y') %>% set_confound(confounds)
#' gbiqq:::make_par_values(model, confound = confounds[1], x = 3)
#' gbiqq:::make_par_values(model, node = 'Y', confound = confounds[1], x = 3)
#'
#' # A residual  confound condition can also be defined
#' gbiqq:::make_par_values(model, confound = list(X = '!(Y[X=1] > Y[X=0])'), x = 3)
#' gbiqq:::make_par_values(model, confound = list(X = '(Y[X=1] == Y[X=0])'), x = 3)
#'
#' # make_par_values can also be used for some vector valued statements
#' model <- make_model('X -> M -> Y')
#' gbiqq:::make_par_values(model, node = c('X', 'Y'), x = 2)
#' gbiqq:::make_par_values(model, label = c('1', '01'), x = 2)
#'
#' # Incompatible conditions produce no change
#' gbiqq:::make_par_values(model, node = 'X', label = '01', x = 2)
#'
#' # If statement not satisfied by any cases then no change
#' model <- make_model("X->Y")
#' gbiqq:::make_par_values(model, statement = '(Y[X=1] == 2)', x = .1)
#'
#' # Normalization: Take in a parameter vector and output is renormalized
#' model <- make_model("X->Y")
#' gbiqq:::make_par_values(model, y = get_parameters(model),
#'   label = '01', x = .1, normalize = TRUE)
#' gbiqq:::make_par_values(model, y = get_parameters(model),
#'   statement = '(Y[X=1] == Y[X=0])', x = .1, normalize = TRUE)
#'
#' # Problematic examples
#' \dontrun{
#' gbiqq:::make_par_values(model, x = 1:2)
#' gbiqq:::make_par_values(model, y = get_parameters(model), label = '01', x = 2, normalize = TRUE)
#' }
#'


make_par_values <- function(model,
														y = get_priors(model),
														x = NA, distribution = NA, node = NA, label = NA, statement = NA,
														confound = NA, nodal_type = NA, param_names = NA, param_set = NA,
														normalize = FALSE) {

	# 1. House keeping

	# 1.0 Data from model
	full_param_set <- model$parameters_df$param_set
	full_node      <- model$parameters_df$node

	# 1.1 Initialize to_alter
	to_alter  <- rep(TRUE, length(y))  # Subset to alter: Starts at full but can get reduced

	# 1.2. If neither a distribution or values vector provided (x) then return existing values (y)
	if (all(is.na(distribution)) & all(is.na(x))) {
		warning("neither distribution nor values provided; no change to values")
		return(y)
	}

	# 1.3. No distribution and values at the same time
	if (!all(is.na(distribution)) & !all(is.na(x))) {
		warning("values and distribution cannot be declared at the same time. try sequentially; no change to values")
		return(y)
	}

	# 1.4. values negative
	if (!all(is.na(x)) && (min(x) < 0))
		stop("x must be non-negative.")


	# 1.5 distribution should be a scalar
	if (max(length(distribution), length(confound), length(statement)) > 1)
		stop("distribution, statement, and confound should be scalars in make_prior_single")

	# 1.6 confound is a named list and if provided only the named node is changed
	if (!is.na(confound) && any(is.na(node)))
		node[is.na(node)] <- names(confound)

	# 1.7 label must be a character
	if (!is.na(label) | !is.na(nodal_type)) {
		if (!is.character(label) && !is.character(nodal_type))
			stop("arguments label and nodal_type must be a character")
	}

	# A. Where to make changes?

	# A1 Do not alter if node is not in listed nodes
	if (!all(is.na(node))) {
		if (!all(node %in% model$nodes))
			stop("listed nodes must be nodes in the model")
		to_alter[!(full_node %in% node)] <- FALSE
	}

	# A2 Do not alter if nodal type is not part of a given statement
	if (!all(is.na(statement))) {
		lnt <- map_query_to_nodal_type(model, statement)
		l_types <- lnt$types

		to_alter[!(full_node %in% lnt$node & model$parameters_df$nodal_type %in% names(l_types[l_types]))] <- FALSE
	}

	# A3 Do not alter if nodal type is not one of listed nodal types
	if (!is.na(nodal_type))
		label <- nodal_type

	if (!all(is.na(label))) {
		to_alter[!(model$parameters_df$nodal_type %in% label)] <- FALSE
	}

	# A4 Do not alter if confound condition is not met: For instance if a condition is 'Y[X=1]>Y[X=0]'
	# then any parameter that does *not* contribute to a causal type satisfying this condition is not
	# modified
	if (!all(is.na(confound))) {
		P_short <- model$P[, map_query_to_causal_type(model, confound[[1]])$types]
		to_alter[(apply(P_short, 1, sum) == 0)] <- FALSE
	}

	# A5 Do not alter if parameter name is not in the model
	if (!all(is.na(param_names))) {
		to_alter[!(model$parameters_df$param_names %in% param_names)] <- FALSE
	}

	# A6 Do not alter if parameter name is not in the model
	if (!all(is.na(param_set))) {
		to_alter[!(full_param_set %in% param_set)] <- FALSE
	}

	# B. What values to provide?  Provide values unless a distribution is provided
	if (!is.na(distribution)) {
		if (!(distribution %in% c("uniform", "jeffreys", "certainty")))
			stop("distribution should be either 'uniform', 'jeffreys', or 'certainty'.")
		x <- switch(distribution, uniform = 1, jeffreys = 0.5, certainty = 10000)
	}


	# C MAGIC

	if (sum(to_alter) == 0) {
		message("No change to values")
		return(y)
	}

	if ((length(x) != 1) & (length(x) != sum(to_alter)))
		stop(paste("Trying to replace", length(to_alter), "parameters with", length(x), "values"))

	# Make changes: No normalization
	y[to_alter] <- x

	# When used for parameters, normalization is usually required
	# We normalize so that pars in a family set that has not been specified renormalize to
	# the residual of what has been specified
	if(normalize) {
		sets_implicated <- unique(full_param_set[to_alter])
		for(j in sets_implicated){
			if(sum(y[to_alter & full_param_set==j])>1) warning("Provided values exceed 1 but normalization requested.")
			y[!to_alter & full_param_set==j] <-
				y[!to_alter & full_param_set==j] * sum(1 - sum(y[to_alter & full_param_set==j])) / sum(y[!to_alter & full_param_set==j])
			y[full_param_set==j] <- y[full_param_set==j]/ sum(y[full_param_set==j])
	   }
	}
	return(y)
}




#' Make par values multiple
#'
#' Internal function to create parameter vector when list like arguments are provided
#'
#' @param model A model created with \code{make_model}
#' @param y Vector of real non negative values to be changed
#' @param x Vector of real non negative values to be substituted into y
#' @param distribution String indicating a common prior distribution (uniform, jeffreys or certainty)
#' @param node A string indicating nodes for which priors are to be altered
#' @param label String. Label for nodal type indicating nodal types for which priors are to be altered
#' @param statement A causal query that determines nodal types for which priors are to be altered
#' @param confound A confound statement that restricts nodal types for which priors are to be altered
#' @param nodal_type String. Label for nodal type indicating nodal types for which priors are to be altered
#' @param param_set String. Indicates the name of the set of parameters to be modified (useful when setting confounds)
#' @param param_names String. The name of specific parameter in the form of, for example, 'X.1', 'Y.01'
#' @param normalize Logical. If TRUE normalizes such that param set probabilities sum to 1.
#'
#' @family priors

make_par_values_multiple <-
	function(model, y=get_priors(model), x = NA, distribution = NA, node = NA, label = NA, statement = NA,
					 confound = NA, nodal_type = NA, param_names = NA, param_set = NA,
					 normalize = FALSE) {

		# Housekeeping regarding argument lengths
		args <- list(distribution = distribution, x = x, node = node, label = label, statement = statement,
								 confound = confound, nodal_type = nodal_type, param_names = param_names, param_set = param_set)
		arg_length   <- unlist(lapply(args, length))

		# Easy case: If all but node, label, or x (replacement values) are of length 1 then simply apply make_par_values
		for (j in c("node", "label", "x", "nodal_type", "param_names", "param_set")) {
			if (max(arg_length[names(args) != j]) == 1)
				return(make_par_values(model, y, x = x, distribution = distribution, node = node,
																			 label = label, statement = statement, confound = confound, nodal_type = nodal_type,
																			 param_names = param_names, param_set = param_set, normalize = normalize))
		}

		# Harder case requires tasks list
		task_list <- make_values_task_list(
			distribution = distribution,
			x = x,
			node = node,
			label = label,
			statement = statement,
			confound = confound,
			nodal_type = nodal_type,
			param_names = param_names,
			param_set = param_set)

		# Magic
		#####################################################

		for (i in 1:ncol(task_list)) {
			y <- with(task_list[, i],

					 	make_par_values(
					 		model, y = y, x = x,
					 		distribution = distribution, node = node, label = label,
					 		statement = statement, confound = confound,
					 		nodal_type = nodal_type, param_names = param_names,
					 		param_set = param_set, normalize = normalize))
		}

		y

	}

