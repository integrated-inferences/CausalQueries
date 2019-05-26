#' Calculate estimand
#'
#' Calculated from a prior or posterior distribution
#'
#'
#' @param model A  model
#' @param lambda A  true parameter vector, if available
#' @param posterior if true use a posterior distribution, otherwise use the prior
#' @param query A query on potential otcomes such as "Y[X=1] - Y[X=0]"
#' @param subset quoted expression evaluates to logical statement. subset allows estimand to be conditioned on *observational* distribution.
#' @param type_distribution if provided saves calculation, otherwise clculated from model; may be based on prior or posterior
#' @export
#' @examples
#' model <- make_model("X" %->% "Y") %>%
#'          set_prior_distribution()
#'  estimand_1  <- get_single_estimand(model, query = "Y[X=1] - Y[X=0]")
#'  estimand_2  <- get_single_estimand(model, query = "Y[X=1] > Y[X=0]")
#'  get_single_estimand(model, lambda = draw_lambda(model), query = "Y[X=1] > Y[X=0]")

get_single_estimand <- function(model,
															 query,
															 subset = TRUE,
															 lambda = NULL, # Use if true parameters known
															 posterior = FALSE,
															 type_distribution = NULL,
															 verbose = TRUE) {


	if(!is.logical(subset)) subset <- with(reveal_outcomes(model),
																				 eval(parse(text = subset)))
	if(all(!subset)) {message("No units in subset"); return() }

	# Evaluation of query on vector of causal types
	x <- (get_types(model, query = query)$types)[subset]


	if(!is.null(lambda)){
		type_distribution <- draw_type_prob(model, lambda = lambda)[subset]
		return(weighted.mean(x, type_distribution))
		}

	if(is.null(type_distribution)) type_distribution <-
		draw_type_prob_multiple(model, posterior = posterior)[subset,]
  	estimand <- apply(type_distribution, 2, function(wt) weighted.mean(x, wt))

  if(verbose) print(paste("mean = ", round(mean(estimand), 3), "; sd = ", round(sd(estimand),3)))

  estimand
}


#' Calculate multiple estimands
#'
#' Calculated from a parameter vector, from a prior or from a posterior distribution
#'
#'
#' @param model A  model
#' @param lambda A  true parameter vector, if available
#' @param posterior if true use a posterior distribution, otherwise use the prior
#' @param subset quoted expression evaluates to logical statement. subset allows estimand to be conditioned on *observational* distribution.
#' @export
#' @examples
#' model <- make_model("X" %->% "Y") %>%
#'            set_prior_distribution()
#'
#' get_estimands(
#'       model,
#'       queries = list(ATE = "Y[X=1] - Y[X=0]", Share_positive = "Y[X=1] > Y[X=0]"))
#'
get_estimands <- function(model,
																				 lambda = NULL,
																				 queries = list(NULL),
																				 subsets = list(TRUE),
																				 posteriors = list(FALSE),
																				 estimand_labels = NULL,
																				 stats = c(mean = mean, sd = sd)){

	if(is.null(estimand_labels)) estimand_labels <- names(queries)

	f <- function(q, subset, posterior){
		v <- get_single_estimand(model, query = q, subset = subset, lambda = lambda, posterior = posterior, verbose = FALSE)
		sapply(stats, function(g) g(v))
	}

	out <- mapply(f, queries, subsets, posteriors)
	rownames(out) <- paste(names(stats))
	if(!is.null(estimand_labels)) colnames(out) <- estimand_labels
	data.frame(out)
}





#' Get data probabilities
#'
#' Takes in a matrix of possible (single case) observations and returns the probability of each
#'
#'
#' @param model A  model
#' @param data Data in long format
#' @export
#' @examples
#' model <- make_model("X" %->% "Y")
#' data <- simulate_data(model, n = 4)
#' get_data_probs(model, data)
get_data_probs <- function(model, data){
	events  <- get_data_events(data = data, model = model)$data_events
	A_w     <- get_likelihood_helpers(model)$A_w
	probs   <- A_w %*% draw_event_prob(model, lambda = model$lambda)
	np      <- rownames(probs)
	unlist(sapply(encode_data(model, data), function(j) probs[np==j]))
	}



#' Conditional inferences
#'
#' Calculate estimands condition on observed data (currently for single case process tracing) together with data realization probabilities
#' Realization probabilities are the probability of the observed data given data is sought onb observed variables
#'@export
#'@examples
#' model <- make_model("X" %->% "Y")
#' model <- set_lambda(model, average = TRUE)
#' conditional_inferences(model, model$lambda, query = "Y[X=1]>Y[X=0]")

conditional_inferences <- function(model, lambda, query, given = NULL){

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

