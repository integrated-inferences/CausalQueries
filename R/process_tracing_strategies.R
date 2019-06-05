#' Get data probabilities
#'
#' Takes in a matrix of possible (single case) observations and returns the probability of each
#'
#' @param model A  model
#' @param data Data in long format
#' @export
#' @examples
#' model <- make_model("X->Y")
#' data <- simulate_data(model, n = 4)
#' get_data_probs(model, data)
get_data_probs <- function(model, data, parameters = NULL){

	if(is.null(parameters)) {
		if(is.null(model$parameters)) stop("parameters not provided")
		parameters <- model$parameters }

	events  <- get_data_events(data = data, model = model)$data_events
	A_w     <- get_likelihood_helpers(model)$A_w
	probs   <- A_w %*% draw_event_prob(model, parameters = parameters)
	np      <- rownames(probs)
	unlist(sapply(encode_data(model, data), function(j) probs[np==j]))
}



#' Conditional inferences
#'
#' Calculate estimands condition on observed data (currently for single case process tracing) together with data realization probabilities
#' Realization probabilities are the probability of the observed data given data is sought onb observed variables
#' @param model A  model
#' @param parameters a parameter vector
#' @param query A query as a character string, for example 'Y[X=1]>Y[X=0]'
#' @param given A conditioning set as a character string that evaluates to a logical, for example 'Y==1'
#'
#' @export
#' @examples
#' model <- make_model("X->Y")
#' model <- set_parameters(model, type = "flat")
#' conditional_inferences(model, query = "Y[X=1]>Y[X=0]")
#'
#' # Example of posteriors given monotonic X -> M -> Y model
#' library(dplyr)
#' model <- make_model("X-> M -> Y")  %>%
#'   set_restrictions(node_restrict = list(M = "10", Y = "10")) %>%
#'   set_parameters(type = "flat")
#' conditional_inferences(model, query = "Y[X=1]>Y[X=0]", given = "Y==1")
#'
#' # Running example
#' model <- make_model("S -> C -> Y <- R <- X; X -> C -> R") %>%
#'    set_restrictions(node_restrict = list(C = "C1110", R = "R0001", Y = "Y0001"), action = "keep")
#' conditional_inferences(model, query = list(COE = "(Y[S=0] > Y[S=1])"), given = "Y==1 & S==0")


conditional_inferences <- function(model, query, parameters=NULL,  given = NULL){

	if(is.null(parameters)) {
		if(is.null(model$parameters)) stop("parameters not provided")
		parameters <- model$parameters }

	vars <- model$variables

	# Possible data
	vals <- data.frame(perm(rep(2,length(model$variables)))) - 1
	vals[vals ==-1] <- NA
	names(vals) <- vars
	if(!is.null(given)) vals <- dplyr::filter(vals, eval(parse(text = given)))

	# Conditions
	conds <- t(apply(vals, 1, function(j) paste(vars, j, sep = "==")))
	conds[is.na(vals)] <- NA
	subsets <- apply(conds, 1, function(j) paste(j[!is.na(j)], collapse = " & "))
	subsets[subsets==""] <- TRUE
	estimands <- gbiqq::get_estimands(
		model   = model,
		parameters  = parameters,
		using = "parameters",
		queries = query,
		subsets = subsets)$mean

	probs <- unlist(get_data_probs(model, vals))

	# hack to deal with fact that get_data_probs returns missing if all NAs
	p <- allNAs <- apply(vals, 1, function(j) all(is.na(j)))
	p[p] <- 1
	p[!p] <- probs

	out <- data.frame(cbind(vals, estimands, p))

	names(out) <- c(vars, "posterior", "prob")
	rownames(out) <- NULL
	data.frame(out)
}



#' Expected learning
#'
#' Expected reduction in variance from one step data collection strategy
#' @param model A  model
#' @param parameters a parameter vector
#' @param query A query as a character string, for example 'Y[X=1]>Y[X=0]'
#' @param strategy A set of variables to be sought
#' @param given A conditioning set as a character string that evaluates to a logical, for example 'Y==1'
#' @importFrom  stringr str_extract_all boundary
#' @export
#' @examples
#' # Reduction in variance given monotonic X -> M1 -> M2 -> Y model
#' library(dplyr)
#' model <- make_model("X -> M1 -> M2 -> Y") %>%
#'   set_restrictions(node_restrict = list(M1 = "10", M2 = "10", Y = "10")) %>%
#'   set_priors() %>%
#'   set_parameters(type = 'flat')
#' el <- expected_learning(model, query = "Y[X=1]>Y[X=0]",
#'                   strategy = c("X", "M2"), given = "Y==1")
#' attr(el, "results_table")
#' el2 <- expected_learning(model, query = "Y[X=1]>Y[X=0]",
#'                   strategy = c("M1"), given = "Y==1 & X==1 & M2==1")
#' attr(el2, "results_table")
#'
#' # No strategy
#' expected_learning(model, query = "Y[X=1]>Y[X=0]")
#'
#' # No givens
#' expected_learning(model, query = "Y[X=1]>Y[X=0]", strategy = c("M1"))
#' expected_learning(model, query = "Y[X=1]>Y[X=0]", strategy = c("M1"), given = "Y==1")
#'
#'
#' library(dplyr)
#'  model <-  make_model("S -> C -> Y <- R <- X; X -> C -> R") %>%
#'  set_restrictions(node_restrict = list(C = "C1110", R = "R0001", Y = "Y0001"), action = "keep")
#' expected_learning(model, query = list(COE = "(Y[S=0] > Y[S=1])"), strategy = "C", given = "Y==1 & S==0")
#' expected_learning(model, query = list(COE = "(Y[X=1] > Y[X=0])"), strategy = "S", given = "X==0 & Y==0")



expected_learning <- function(model, query, strategy = NULL, given = NULL, parameters = NULL){

		vars <- model$variables
		given0 <- ifelse(is.null(given), " ", given)

		# Figure out which variables are given
		given_vars <- NULL
		if(!is.null(given)) {
			given_vars <- str_extract_all(given, boundary("word"))[[1]]
			given_vars <- given_vars[(given_vars %in% vars)]}

		# All strategy vars need to be seen
		if(!is.null(strategy)){
			vars_to_see <- paste0("!is.na(", strategy, ")", collapse = " & ")
			if(is.null(given)) { given <- vars_to_see
			} else {
			given <- paste(given, "&", vars_to_see)}
			}

		# Augment "given" to examine cases with NA in all other vars
    unseen_vars <- vars[!(vars %in% c(strategy, given_vars)) ] # na only for these vars
		if(length(unseen_vars) >0) {
			unseen <- paste0("is.na(", unseen_vars, ")", collapse = " & ")
			if(is.null(given)) {given <- unseen} else {given  <- paste(given, "&", unseen)}
			}

    ######################

	results_table <-
		conditional_inferences(model = model, query = query,
													 given = given, parameters = parameters)
  results_table <- filter(results_table, prob !=0)
	# Clean up
	results_table <- mutate(results_table,  prob = prob/sum(prob), var = posterior*(1-posterior))

	# Summarize
  out <- with(results_table,
  						data.frame(
  							strategy = paste(strategy, collapse = ", "),
  							given = given0,
  							prior_estimand = prob%*%posterior,
  							prior_var  = (prob%*%posterior)*(1- prob%*%posterior),
  							E_post_var = (prob%*%var), stringsAsFactors = FALSE))

#  print(query)
#  print(out)

  attr(out, "results_table") <- results_table

  out
}


