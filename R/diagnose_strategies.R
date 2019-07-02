#' Produces list of possible data given model and data strategy
#'
#' THIS IS CURRENTLY NOT AT ALL GENERALY AND NEEDS TO BE MADE GENERAL FOR A DATA STRATEGY FO THE FORM:
#' "GATHER DATA ON X,M,Y FOR N MORE CASES.... or GATHER DATA ON M in N OF THE X=Y=1 CASES....
#'
#' @param model A causal model as created by \code{make_model}
#' @param given A data frame with observations
#' @param data_strat data strategy. If NULL it gathers data for all possible cases.
#' @param vars_to_seek Variables to be sought or NA. If NA \code{make_possible_data} gathers data on all variables containing NA for the specified data strategy.
#' @export
#' @return A dataset
#' @examples
#' library(dplyr)
#' model <- make_model("X->M->Y")  %>%
#'    set_restrictions(causal_type_restrict = "Y[M=1]<Y[M=0] | M[X=1]<M[X=0] ") %>%
#'    set_parameter_matrix()
#' # Look for data on M for all possible cases in the given data
#' given <- data.frame(X = c(0,0,0,1,1,1), M = NA, Y = c(0,0,1,0,1,1))
#' make_possible_data(model, given)
#'
#' # Look for data on M  when  X = Y = 1
#' make_possible_data(model, given, data_strat = X == 1 & Y == 1)
#'
#'# Look for data on M  when X=1 or Y==1
#' make_possible_data(model, given, data_strat = X == 1 | Y == 1)
#'
#' # Look for data on K and M
#' model <- make_model("X->M->Y <-K")   %>%
#'    set_parameter_matrix()
#' given <- data.frame(X = c(0,0,0,1,1,1), K = NA, M = NA, Y = c(0,0,1,0,1,1))
#' make_possible_data(model, given)
#' # Look for data only on M
#' make_possible_data(model, given, vars_to_seek = "M")
#' # Look for data only on M when  X = 1 and Y = 0
#' make_possible_data(model, given, data_strat =  X == 1 & Y == 0, vars_to_seek = "M")
make_possible_data <- function(model,
															 given,
															 data_strat   = list(NULL),
															 vars_to_seek = list(NA)){
	data_strat_expr <- as.list(substitute(data_strat))
	data_strat_expr <- data_strat_expr[2:length(data_strat_expr)]
	n <- max(length(data_strat_expr), length(vars_to_seek))
	#data_strat   <- as.list(data_strat)
	vars_to_seek <- as.list(vars_to_seek)

	if(!identical(length(	data_strat_expr), length(vars_to_seek))){
	  if (length(	data_strat_expr) != 1 & length(vars_to_seek) !=1)
	  	stop("data_strat and vars_to seek should either have the same length or one of them must be of legnth 1L")
	  if(length(vars_to_seek) > length(	data_strat_expr)) {
	  	data_strat <- as.list(rep(	data_strat_expr, n))
	  } else{
	  	vars_to_seek <- as.list(rep(vars_to_seek , n))
	  }
	}



	possible_datasets <- lapply(1:n, function(i){
		make_possible_data_single(model, given, data_strat = 	expr(!!data_strat_expr[[i]]), vars_to_seek = vars_to_seek[[i]])
	})

 out_possible_data <- do.call(possible_datasets, merge, by = "event")
 out_possible_data <- possible[,!duplicated(t(out_possible_data))]

 return(out_possible_data)
}
#' Make possible data for a single strategy
#'
make_possible_data_single <- function(model,
															 given,
															 data_strat = NULL, vars_to_seek = NA) {

	#
	#  possible0 <- sapply(1:nrow(given), function(j) {W2 <- given;
	#  W2[j, 2] <- 0;
	#  as.numeric(trim_strategies(model, W2)[,3])})
	#  possible0 <- possible0[,!duplicated(t(possible0))]
	#
	#  possible1 <- sapply(1:nrow(given), function(j) {W2 <- given;
	#  W2[j, 2] <- 1;
	#  as.numeric(trim_strategies(model, W2)[,3])})
	#  possible1 <- possible1[,!duplicated(t(possible1))]
	#
	#  W2 <- given; W2[1, 2] <- 0
	#  none <- c(rep(0, 8), trim_strategies(model, given)$count)
	W2 <- w_given   <- given
	data_strat_expr <- enexpr(data_strat)
	variables       <- vars_to_seek
	if(!eval_bare(is_null(data_strat_expr))){
	w_given <- eval_tidy(dplyr::filter(given, !!!data_strat))
	}

 possible_value <- function(given, value, vars_to_seek, variables){
  W2 <- given
  	possible <- sapply(1:nrow(given), function(j) {
  		if(is.na(vars_to_seek)){
  			variables <- which(is.na(W2[j,]))}
  		W2[j, variables] <- value;
  		as.numeric(trim_strategies(model, W2)[,3])})

    possible[,!duplicated(t(possible))]
  }

  possible0 <- possible_value(given = w_given, value = 0, vars_to_seek, variables)
  possible1 <- possible_value(given = w_given, value = 1, vars_to_seek, variables)

  if(is.na(vars_to_seek)){
  	W2[apply(W2,2, is.na)] <- 0
  } else{
   W2[, variables] <- 0
  }

 # w_given <- rbind(w_given, W2)

	possible_data <- cbind(trim_strategies(model,W2)[,1:2],
												possible0, possible1
												#, none = none
	)
	colnames(possible_data)[3:ncol(possible_data)] <-1:length(3:ncol(possible_data))
	possible_data
}



#' Generates a probability distribution over possible data outcomes
#'
#' THIS IS NOT GENERAL AT ALL YET. THE BASIC IDEA THOUGH IS TO USE draw_event_prob
#' THOUGH IN PRACTICE WE WILL LIKELY NEED dmultinom
#'
#' @param model A causal model as created by \code{make_model}
#' @param given A data frame with observations
#' @param data_strat data strategy
#' @export
#' @return A dataset
#' @examples
#'
#' library(dplyr)
#' model <- make_model("X->M->Y")  %>%
#'    set_restrictions(causal_type_restrict = "Y[M=1]<Y[M=0] | M[X=1]<M[X=0] ") %>%
#'    set_parameter_matrix()
#'
#' given = data.frame(X = c(0,0,0,1,1,1), M = NA, Y = c(0,0,1,0,1,1))
#' pars <- draw_parameters(model)
#' make_data_probabilities(model, given, data_strat = c(1,3), pars = pars)
#'
make_data_probabilities <- function(model, given, data_strat = NULL, pars) {

	event_prob <- draw_event_prob(model, parameters = pars)

	x <- rep(0, length(event_prob))
	x[data_strat] <- event_prob[data_strat]
	x / sum(x)
}


#' Generates a database of results using gbiqq over possible data
#'
#' This function runs many models and can take a long time depending on the size of possible data.
#'
#' @param model A causal model as created by \code{make_model}
#' @param possible_data A data frame with possible data
#' @param queries Queries
#' @export
#' @return A dataframe
#' @examples
#' #' library(dplyr)
#' model <- make_model("X->M->Y")  %>%
#'    set_restrictions(causal_type_restrict = "Y[M=1]<Y[M=0] | M[X=1]<M[X=0] ") %>%
#'    set_parameter_matrix()
#'
#' given = data.frame(X = c(0,0,0,1,1,1), M = NA, Y = c(0,0,1,0,1,1))
#'
#' possible_data <- make_possible_data(model, given)
#'
#' estimates_database <- make_estimates_database(
#'       model,
#'       given = given,
#'       possible_data,
#'       queries = "Y[X=1]>Y[X=0]")
#'

make_estimates_database <- function(model,
																		given,
																		possible_data = NULL,
																		queries = "Y[X=1]>Y[X=0]",
																		data_strat = NULL) {

	if(!exists("fit")) fit  <- fitted_model()
	if(is.null(possible_data)) possible_data <- make_possible_data(model, given, data_strat)

	## HACK: 3 here only because of particular shape of possible data
	out <- sapply(3:ncol(possible_data), function(j) {

		data_events <- possible_data[, c(1, j)]

		data <- simulate_data(model, data_events = data_events)

		updated <- gbiqq::gbiqq(model = model, data = data, stan_model = fit)

		gbiqq::get_estimands(updated,
												 queries = queries,
												 using = "posteriors",
												 subset = TRUE)

	})
	## NEED BETTER OUTPUT FORMAT? NAD INCLUDE SUMMARY OF DATA IN TEH OUTPUT DATABASE TO IMPROVE LEGIBILITY
	t(out)
}
## NOTE NEED TO ADD SUBSET ARGUMENT
##  gbiqq::gbiqq necessary for me for reasons I don't understand!



#' Diagnosis a data strategy
#'
#' @param reference_model A causal model as created by \code{make_model}
#' @param analysis_model A causal model as created by \code{make_model}
#' @param given A data frame with exisitng data
#' @param queries queries
#' @export
#' @return A dataframe
#' @examples
#'
#' library(dplyr)
#' reference_model <- analysis_model <-
#'    make_model("X->M->Y")  %>%
#'    set_restrictions(causal_type_restrict = "Y[M=1]<Y[M=0] | M[X=1]<M[X=0] ") %>%
#'    set_parameter_matrix()
#'
#' given = data.frame(X = c(0,0,0,1,1,1), M = NA, Y = c(0,0,1,0,1,1))
#'
#' diagnose_strategy(reference_model, analysis_model,
#'                   given = given, queries = "Y[X=1]>Y[X=0]",
#'                   data_strat = c(1,3),
#'                   estimates_database = estimates_database,
#'                   sims = 10)

diagnose_strategy <- function(reference_model,
															analysis_model,
															given,
															queries,
															data_strat = NULL,
															estimates_database = NULL,
															possible_data = NULL,
															sims = 1000) {
	# Houskeeping

	if(!exists("fit")) fit  <- fitted_model()

	if(is.null(possible_data)) possible_data <-
			make_possible_data(reference_model, given, data_strat)

	if(is.null(estimates_database)) {
		fit  <- fitted_model()
		estimates_database <- make_estimates_database(model, possible_data, queries = queries)
	  }

	# The magic: Draw multiple parameters and get mean squared error over possible data observations as well as posterior variance.

	replicate(sims,
		{
		# draw parameters
		using <- ifelse(is.null(reference_model$posterior_distribution), "priors", "posteriors")
		pars   <- draw_parameters(reference_model, using = using)

		# implied data probabilities
		probs <- make_data_probabilities(reference_model, given, data_strat, pars)

		# implied estimand
		estimand <- gbiqq::get_estimands(reference_model,
																		 parameters = pars,
																		 queries = queries,
																		 using = "parameters",
																		 subset = TRUE)$mean
		# evaluation
		estimates     <- unlist(estimates_database[,"mean"])
		squared_error <- (estimates - estimand)^2
		post_var      <- (unlist(estimates_database[,"sd"]))^2

		# Return
		c(estimand = estimand,
			estimate = estimates%*%probs,
			MSE      = squared_error%*%probs,
			post_var = post_var%*%probs)
		})
	}
