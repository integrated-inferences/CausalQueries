#' Produces list of possible data given model and data strategy
#'
#' THIS IS CURRENTLY NOT AT ALL GENERALY AND NEEDS TO BE MADE GENERAL FOR A DATA STRATEGY FO THE FORM:
#' "GATHER DATA ON X,M,Y FOR N MORE CASES.... or GATHER DATA ON M in N OF THE X=Y=1 CASES....
#'
#' @param model A causal model as created by \code{make_model}
#' @param given A data frame with observations
#' @param cases  A list of character strings indicating for which cases data should be gathered. Options are: (i) to gather additional data on variables specified via \code{vars} for any possible cases in the model ("any"), (ii) to gather data in all cases within a given dataset ("within"), or (iii) to specify the subset of cases for which within-case data should be collected (e.g. "Y == 1").
#'  @param vars Variables to be sought or NA. If NA \code{make_possible_data} gathers data on all variables containing NA for the specified data strategy.
#' @param direction. . It gets overridden by \code{subset} when \code{subset} is not NULL.
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
#'  # Gather data on X and Y
#' make_possible_data(model,  N = list(4), vars = list(c("X", "Y")))
#' # Look for data on M  when  X = Y = 1
#' make_possible_data(model, given, cases = list("X == 1 & Y == 1"))
#'
#'# Look for data on M  when X=1 or Y==1
#' make_possible_data(model, given, cases = "X == 1 | Y == 1")
#'
#' # Look for data on K and M
#' model <- make_model("X->M->Y <-K")   %>%
#'    set_parameter_matrix()
#' given <- data.frame(X = c(0,0,0,1,1,1), K = NA, M = NA, Y = c(0,0,1,0,1,1))
#' make_possible_data(model, given)
#' # Look for data only on M for all within-cases
#' make_possible_data(model, given, vars = list("M"), cases = list("within"), N = list(nrow(given)))
#' # Look for data on M when X = 1 and Y = 0
#' make_possible_data(model, given, cases =  "X == 1 & Y == 0", vars = list("M"))
make_possible_data <- function(model,
															 given = NULL,
															 N = list(1),
															 cases   = list("any"),
															 vars = list(NA)
															 ){


	if(!identical(length(	cases), length(vars), length(N)) )
	  	stop("N, cases and vars must have the same length")

	# to do: combine given with possible data sets produced for N and use combined ds in subsequent steps.
	# main complication is that the shapes of the datasets are long and wide
	out_possible_data <- possible_datasets <- lapply(1:length(cases), function(i){
			make_possible_data_single(model,
															given,
															N     = N[[i]],
															cases = cases[[i]],
															vars  = vars[[i]])
	})



 return(out_possible_data)
}
#' Make possible data for a single strategy step
#'
make_possible_data_single <- function(model,
															 given = NULL,
															 N = NULL,
															 cases = NULL,
															 vars = NA) {


	# The script inside the conditional below gets the max possible data
	# when N obs of correlation data are collected on "vars"
	# each col in possible_data represents a possible dataset that might be observed
	# if we were to gather N obs on vars
	if(cases == "any"){
 	possible <- get_max_possible_data(model)
 	if(!all(is.na(vars))) possible[, !names(possible) %in% vars] <- NA
 	d.frame  <- trim_strategies(model, possible)[,1:2]
 	possible_data_perm <- perm(rep(N, length(d.frame$event)))
 	n_tot <- rowSums(possible_data_perm)
  possible_data <- 	possible_data_perm[n_tot==(N),]
  possible_data <-  as.data.frame(t(possible_data))
  possible_data <-  cbind(d.frame  ,possible_data)
  colnames(possible_data)[3:ncol(possible_data)] <-1:length(3:ncol(possible_data))


 # The script inside the else below gets the max possible data
 # when cases == "within" or when cases specifies a subset
 }else {

 # Function to assign possible 0 or 1
 possible_value <- function(given, value, vars,  i_cases = NULL){
  W2 <<- given
  variables <- vars
  i_cases <- ifelse(is.null(i_cases), 1:nrow(given), i_cases )
  	possible <- sapply(i_cases, function(j) {
  		if(length(vars) == 1){
  			if(is.na(vars))
  			variables <- which(is.na(W2[j,]))}
  		W2[j, variables] <<- value;
  		as.numeric(trim_strategies(model, W2)[,3])})

    possible[,!duplicated(t(possible))]
 }

 if(cases == "within"){

 	# to fix: it gathers data on any variable specified via vars even if variable observed in given. should throw error or warning

 	if(!is.null(N) & (N > nrow(given)) )
 		stop("N must be less or equal than the number of rows in given when case equals `within`.")

 	combinations <- data.frame(1:nrow(given))
 	# Possible combinations of cases for which one could gather data
 	if(!is.null(N)) combinations <- combn(1:nrow(given), N)
  possible0 <- sapply(1:ncol(combinations),function(x) possible_value(given, value = 0, vars,combinations[,x]))
  possible1 <- sapply(1:ncol(combinations),function(x) possible_value(given, value = 1, vars,combinations[,x]))
  possible0 <- possible0[,!duplicated(t(possible0))]
  possible1 <- possible1[,!duplicated(t(possible1))]


  possible_data <- cbind(trim_strategies(model,W2)[,1:2],
  											 possible0, possible1)

  colnames(possible_data)[3:ncol(possible_data)] <-1:length(3:ncol(possible_data))
	} else {
    # if cases specifies a subset
		w_given <- subset(given, eval(parse(text = cases)))
		possible0 <- possible_value(given = w_given, value = 0, vars)
		possible1 <- possible_value(given = w_given, value = 1, vars)
		possible_data <- cbind(trim_strategies(model,W2)[,1:2],
													 possible0, possible1)
		colnames(possible_data)[3:ncol(possible_data)] <-1:length(3:ncol(possible_data))
  }
 }

	return(possible_data)
}



#' Generates a probability distribution over possible data outcomes
#'
#' THIS IS NOT GENERAL AT ALL YET. THE BASIC IDEA THOUGH IS TO USE draw_event_prob
#' THOUGH IN PRACTICE WE WILL LIKELY NEED dmultinom
#'
#' @param model A causal model as created by \code{make_model}
#' @param given A data frame with observations
#' @param subset data strategy
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
#' make_data_probabilities(model, given, subset = c(1,3), pars = pars)
#'
make_data_probabilities <- function(model, given, subset = NULL, pars) {

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
