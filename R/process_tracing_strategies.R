#' Get data probabilities
#'
#' Takes in a matrix of possible (single case) observations and returns the probability of each
#'
#' @param model A  model
#' @param data Data in long format
#' @export
#' @examples
#' model <- make_model("X" %->% "Y")
#' data <- simulate_data(model, n = 4)
#' get_data_probs(model, data)
get_data_probs <- function(model, data, lambda = NULL){

	if(is.null(lambda)) {
		if(is.null(model$lambda)) stop("lambda not provided")
		lambda <- model$lambda }

	events  <- get_data_events(data = data, model = model)$data_events
	A_w     <- get_likelihood_helpers(model)$A_w
	probs   <- A_w %*% draw_event_prob(model, lambda = lambda)
	np      <- rownames(probs)
	unlist(sapply(encode_data(model, data), function(j) probs[np==j]))
}



#' Conditional inferences
#'
#' Calculate estimands condition on observed data (currently for single case process tracing) together with data realization probabilities
#' Realization probabilities are the probability of the observed data given data is sought onb observed variables
#' @param model A  model
#' @param lambda a parameter vector
#' @param query A query as a character string, for example 'Y[X=1]>Y[X=0]'
#' @param given A conditioning set as a character string that evaluates to a logical, for example 'Y==1'
#'
#' @export
#' @examples
#' model <- make_model("X" %->% "Y")
#' model <- set_lambda(model, average = TRUE)
#' conditional_inferences(model, query = "Y[X=1]>Y[X=0]")
#'
#' # Example of posteriors given monotonic X -> M -> Y model
#' library(dplyr)
#' model <- make_model("X" %->% "M", "M" %->% "Y") %>%
#' set_restrictions(node_restrict = list(M = "10", Y = "10")) %>%
#' set_lambda(average = TRUE)
#' conditional_inferences(model, query = "Y[X=1]>Y[X=0]", given = "Y==1")

conditional_inferences <- function(model, query, lambda=NULL,  given = NULL){

	if(is.null(lambda)) {
		if(is.null(model$lambda)) stop("lambda not provided")
		lambda <- model$lambda }

	vars <- model$variables

	# Possible data
	vals <- data.frame(perm(rep(3,length(model$variables)))) - 1
	vals[vals ==-1] <- NA
	names(vals) <- vars
	if(!is.null(given)) vals <- dplyr::filter(vals, eval(parse(text = given)))

	# Conditions
	conds <- t(apply(vals, 1, function(j) paste(vars, j, sep = "==")))
	conds[is.na(vals)] <- NA
	subsets <- apply(conds, 1, function(j) paste(j[!is.na(j)], collapse = " & "))
	subsets[subsets==""] <- TRUE
	estimands <- get_estimands(
		model   = model,
		lambda  = lambda,
		queries = query,
		subsets = subsets)[1,]

	probs <- unlist(get_data_probs(model, vals))

	# hack to deal with fact that get_data_probs returns missing if all NAs
	p <- allNAs <- apply(vals, 1, function(j) all(is.na(j)))
	p[p] <- 1
	p[!p] <- probs

	out <- data.frame(cbind(vals, t(estimands), p))

	names(out) <- c(vars, "posterior", "prob")
	rownames(out) <- NULL
	data.frame(out)
}



#' Expected learning
#'
#' Expected reduction in variance from one step data collection strategy
#' @param model A  model
#' @param lambda a parameter vector
#' @param query A query as a character string, for example 'Y[X=1]>Y[X=0]'
#' @param strategy A set of variables to be sought
#' @param given A conditioning set as a character string that evaluates to a logical, for example 'Y==1'
#'
#' @export
#' @examples
#' # Reduction in variance given monotonic X -> M -> Y model
#' library(dplyr)
#' model <- make_model("X" %->% "M1", "M1" %->% "M2", "M2" %->% "Y") %>%
#'   set_restrictions(node_restrict = list(M1 = "10", M2 = "10", Y = "10")) %>%
#'   set_lambda(average = TRUE)
#' el <- expected_learning(model, query = "Y[X=1]>Y[X=0]",
#'                   strategy = c("X", "M2"), given = "Y==1")
#' attr(el, "results_table")

expected_learning <- function(model, query, strategy, given = NULL, lambda = NULL){

	results_table <-
		conditional_inferences(model = model, query = query,
													 given = given, lambda = lambda)

	# Retain rows with data in strategy variables
	strategy_rows <- paste0("!is.na(", strategy, ")", collapse = " & ")
	results_table <- filter(results_table, !!parse_expr(strategy_rows))

	# Remove rows with data in non strategy / non given variables
	# Figure out which vars are in the given statement
	# NOTE: SPPED UP BY ADDING UNWANTED VARS TO THE GIVENS
	if(!is.null(given)) {
		vars <- model$variables
		poss_vars <- stringr::str_extract_all(given, stringr::boundary("word"))[[1]]
		unseen_vars <- vars[!(vars %in% c(poss_vars,strategy)) ] # na only for these vars
		if(length(unseen_vars)>0){
	  	strategy_rows <- paste0("is.na(", unseen_vars, ")", collapse = " & ")
	  	results_table <- filter(results_table, !!parse_expr(strategy_rows))
			}
  	}

	# Clean up
	results_table <- mutate(results_table,  prob = prob/sum(prob), var = posterior*(1-posterior))

	# Summarize
  out <- with(results_table,
  						data.frame(
  							given = given, strategy = paste(strategy, collapse = ", "),
  							prior_estimand = prob%*%posterior,
  							prior_var  = (prob%*%posterior)*(1- prob%*%posterior),
  							E_post_var = (prob%*%var)))

  print(query)
  print(out)

  attr(out, "results_table") <- results_table

  out
}


