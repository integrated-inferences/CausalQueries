#' Make  data for multi-step strategy
#'
#' Creates a database of possible data from a data strategy.
#' Users can gather additional data on variables specified via \code{vars} for any possible cases in the model ("any"). Or they can
#' gather data in all cases within a given dataset ("within"). Or they can specify  the subset of cases for which within-case data should be collected (e.g. "Y == 1").
#'
#' @param model A causal model as created by \code{make_model}
#' @param given A data frame with observations
#' @param N Number of variables to seek
#' @param within logical Whether to seek variables within existing data
#' @param condition  A list of character strings indicating for which cases data should be gathered. Options are: (i) to gather additional data on variables specified via \code{vars} for any possible cases in the model ("any"), (ii) to gather data in all cases within a given dataset ("within"), or (iii) to specify the subset of cases for which within-case data should be collected (e.g. "Y == 1").
#' @param vars Variables to be sought or NA. If NA \code{make_possible_data} gathers data on all variables containing NA for the specified data strategy.
#' @export
#' @return A dataset
#' @examples
#' library(dplyr)
#' model <- make_model("X->M->Y")  %>%
#'    set_restrictions(causal_type_restrict = "Y[M=1]<Y[M=0] | M[X=1]<M[X=0] ") %>%
#'    set_parameter_matrix()
#' df <- data.frame(X = c(0,0,0,1,1,1), M = NA, Y = c(0,0,1,0,1,1))
#' given <- trim_strategies(model, df)[, -2]
#'
#' # Look for data on M for all possible cases in the given data
#' make_possible_data(model, N = 2)
#' make_possible_data(model, given, within = TRUE, N = 2)
#'
#' # Not possible:
#' make_possible_data(model, given, within = TRUE, N = 7)
#'
#' # Within conditions
#' make_possible_data(model, given, within = TRUE, N = 2, condition = "X==1 & Y==1")
#' make_possible_data(model, given, within = TRUE, N = 3, condition = "Y==1")
#' make_possible_data(model, given, within = TRUE, condition = "X == 1 | Y == 1")
#'
#' # Look for data on K but not M
#' # THIS IS NOT WORKING YET
#' model <- make_model("X->M->Y <-K")   %>%
#'    set_parameter_matrix()
#' df <- data.frame(X = c(0,0,1,1,1), K = NA, M = NA, Y = c(0,0,0,1,1))
#' given <- trim_strategies(model, df)[, -2]
#' #make_possible_data(model, given, within = TRUE, N = 1, vars = "K")
#'
#' # Look for data on M when X = 1 and Y = 0
#' make_possible_data(model,
#'                    given,
#'                    condition =  "X == 1 & Y == 0",
#'                    vars = list("M"))
#'
#' model <- make_model("X->M->Y")   %>%
#'    set_parameter_matrix()
#'make_possible_data(model,
#'                    given = NULL,
#'                    N = list(3,1),
#'                    within = FALSE,
#'                    condition =  list(TRUE, "X == 1 & Y == 0"),
#'                    vars = list(NULL, NULL))

make_possible_data <- function(model,
															 given = NULL,
															 N = list(1),
															 within = FALSE,
															 condition = list(TRUE),
															 vars = list(NULL)) {

	if(!is.null(given)) if(!identical(names(given), c("event", "count"))){
		stop("'given' df should have two columns: event and count")}

	if(!identical(length(condition), length(vars), length(N)) )
		stop("N, cases and vars must have the same length")

	given <- gbiqq:::make_possible_data_single(model,
																		 given = given,
																		 within = within,
																		 N = N[[1]],
																		 condition = condition[[1]],
																		 vars = vars[[1]] )

	if(length(N) == 1) return(given)

	for(i in 2:length(N)) {
		out <- sapply(3:ncol(given), function(s) {
			g_df <- given[,c(1,s)]
			names(g_df) <- c("event", "count")
			make_possible_data_single(model,
																given = g_df,
																within = TRUE,
																N     = N[[i]],
																condition = condition[[i]],
																vars  = vars[[i]])[, -1]
			})
		# rbind not working yet since output is of different length
			x <- do.call("rbind", out)
			x <- t(t(x)[!duplicated(t(x)),])
			given <- cbind(given[,1:2], x)
	}
	given
	}




#' Make possible data for a single strategy step
#'
#' Creates a database of possible data from a data strategy.
#' Users can gather additional data on variables specified via \code{vars} for any possible cases in the model ("any"). Or they can
#' gather data in all cases within a given dataset ("within"). Or they can specify  the subset of cases for which within-case data should be collected (e.g. "Y == 1").
#'
#' @param model A causal model as created by \code{make_model}
#' @param given A data frame in compact form with first column indicating event type and second column indicating number of events of that type.
#' @param N Number of variables to seek
#' @param within logical Whether to seek variables within existing data
#' @param condition  A list of character strings indicating for which cases data should be gathered. Options are: (i) to gather additional data on variables specified via \code{vars} for any possible cases in the model ("any"), (ii) to gather data in all cases within a given dataset ("within"), or (iii) to specify the subset of cases for which within-case data should be collected (e.g. "Y == 1").
#' @param vars Variables to be sought or NA. If NA \code{make_possible_data} gathers data on all variables containing NA for the specified data strategy.
#' @export
#' @return A dataset
#' @examples
#' library(dplyr)
#' model <- make_model("X->M->Y")  %>%
#'    set_restrictions(causal_type_restrict = "Y[M=1]<Y[M=0] | M[X=1]<M[X=0] ") %>%
#'    set_parameter_matrix()
#' df <- data.frame(X = c(0,0,0,1,1,1), M = NA, Y = c(0,0,1,0,1,1))
#' given <- trim_strategies(model, df)[, -2]
#'
#' # Look for data on M for all possible cases in the given data
#' make_possible_data_single(model, N = 2)
#' make_possible_data_single(model, given = given, within = TRUE, N = 2)
#' make_possible_data_single(model, given = given,
#'                           within = TRUE,
#'                           N = 2,
#'                           condition = "X==1 & Y==1")
#'
#' model <- make_model("X->M->Y <- K")  %>%
#'    set_restrictions(causal_type_restrict = "(Y[M=1, K= .]<Y[M=0, K= .]) | M[X=1]<M[X=0] ") %>%
#'    set_parameter_matrix()
#' df <- data.frame(X = c(0,0,0,1,1,1), K = NA,  M = NA, Y = c(0,0,1,0,1,1))
#' given <- trim_strategies(model, df)[, -2]
#' make_possible_data_single(model, given = given,
#'                           within = TRUE,
#'                           N = 2,
#'                           condition = "X==1 & Y==1",
#'                           vars = "M")
#'
#'
make_possible_data_single <- function(model,
																			given = NULL,
																			N = 1,
																			within = FALSE,
																			condition = TRUE,
																			vars = NULL) {

  if(within & is.null(given)) stop("If 'within' is specified 'given' must be provided")

	if(!within){possible_data <-  gbiqq:::all_possible(model, N, vars)}

	if(within){

		if(is.null(given)) stop("given not provided, but 'within' requested")

		possible <- get_max_possible_data(model)
		#		possible <- all_data_types(model)  # LM--use this instead to condition on NA values
		#                                      #We can look in a bin if it satisfied condition *and* we do not already have data on at least some vars
		#		if(!is.null(vars)) condition <-
		#				paste0("(", condition, ") & (", paste0("is.na(", vars, ")", collapse = "|"), ")")

		possible <- possible[with(possible, eval(parse(text = condition))),]
		possible <- collapse_data(possible, model)
		A_w           <- get_likelihood_helpers(model)$A_w

		# What is the set of types in which we can seek new data
		acceptable_bucket <- (A_w %*% possible[,2])>0
		acceptable_bucket <-rownames(acceptable_bucket)[acceptable_bucket]

		buckets <- given
		buckets$capacity <- buckets$count
		buckets$capacity[!(given$event %in% acceptable_bucket)] <-0

		if(sum(buckets$capacity) < N) {message("Not enough space to allocate N"); return(given)}

		buckets <- cbind(buckets, as.matrix(partitions::blockparts(buckets$capacity, N)))

		## STOP HERE

		possible_data <-
		gbiqq:::all_possible(model, N, vars = vars, condition = condition)[, -2]

		# Need to figure out if these are consistent with `given`` and if so, merge
		A_w           <- get_likelihood_helpers(model)$A_w

		# given in long form
		# make_long <- function(model, given){
		# 	A_w           <- get_likelihood_helpers(model)$A_w
		# 	long_given <- data.frame(event = rownames(A_w))
		# 	long_given <- merge(long_given, given, by = "event", all.x = TRUE)
		# 	long_given$count[is.na(long_given$count)] <-0
		# 	long_given
		# }
		# make_long(model, given)

		# Reclassify sampled data as different data types
		poss_types    <- A_w%*%as.matrix(possible_data[,-1])

		# Check consistency
		consistent   <- poss_types[rownames(poss_types) %in%	given$event,]
		admissible   <- apply(consistent, 2, function(j) all(j <= given$count))
		if(!any(admissible)) {message("N too large given constraints"); return(given)}
		within_cases <- possible_data[, c(TRUE, admissible)]

		# Data types for which further within-case data is not sought
		residual_cases <- apply(consistent, 2, function(j) given$count - j)[, admissible]
		residual_cases <- data.frame(event = given$event, residual_cases)
		names(residual_cases) <- names(within_cases)

		# Combine within cases with residual cases
		possible_data <- rbind(within_cases, residual_cases)
		rownames(possible_data) <- NULL
		colnames(possible_data) <- c("event", 1:(ncol(possible_data)-1))

	}

	return(possible_data)
}



#' helper for ways to allocate N units into n data types: tidies partition::composition output
#'
#' @param N Number of observations to be distributed
#' @param n Number of possible values observations could take
#' @examples
#' allocations(4,2)
allocations <- function(N, n) {
	x <- partitions::compositions(N,n)
	x <- data.frame(as.matrix(x))
	colnames(x) <- 1:ncol(x)
	x
 }


#' helper for getting all data on variables with N observed
#'
#' @examples
#' model <- make_model("X->M->Y")
#' gbiqq:::all_possible(model, N=2, vars = c("X", "M"))
#' gbiqq:::all_possible(model, N=2, vars = c("X", "Y"), condition = "Y==0")
all_possible <- function(model, N, vars = NULL, condition = TRUE){

	if(is.null(vars)) vars <- model$variables
	possible               <- get_max_possible_data(model)
	if(!all(is.na(vars))) possible[, !names(possible) %in% vars] <- NA
  possible <- possible[with(possible, eval(parse(text = condition))),]

	d.frame   <- trim_strategies(model, possible)
	df        <- trim_strategies(model, possible)[d.frame$count >0 ,1:2]

	possible_data <- gbiqq:::allocations(N, length(df$event))
	out <- matrix(0, nrow(d.frame), ncol(possible_data))
	out[d.frame$count >0,] <- as.matrix(possible_data)
	cbind(d.frame[,1:2], out)
  }

#' Generates a probability distribution over possible data outcomes
#'
#' NOTE: This needs to be checked for whether it is taking account of strategy probabilities properly
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
#' pars <- draw_parameters(model)
#' possible_data <- make_possible_data(model, N= 2)
#' make_data_probabilities(model, pars = pars, possible_data)
#'
#' given <- data.frame(X = c(0,0,0,1,1,1), M = NA, Y = c(0,0,1,0,1,1))
#' possible_data <- make_possible_data(model, given = given, cases = "X==1 & Y==1")
#' make_data_probabilities(model, pars = pars, possible_data)
#'
make_data_probabilities <- function(model, pars,  possible_data) {

	A_w <- (get_likelihood_helpers(model)$A_w)[possible_data$event, ]
	w   <-  draw_event_prob(model, parameters = pars, using = "parameters")
  w_full = A_w %*% w

  strategy   <- possible_data$strategy
  strat_set <- unique(strategy)

  # Probability of outcomes within each strategy set
	x <- apply(possible_data[,-(1:2)], 2, function(d)
	sapply(strat_set, function(j) dmultinom(d[strategy==j],
																					prob = w_full[strategy==j])
				 ))
	if(!is.null(nrow(x))) x <- apply(x, 2, prod)

	# Normalization
	x/sum(x)
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
