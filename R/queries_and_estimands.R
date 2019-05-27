#' Calculate estimand distribution
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
#' model <- make_model("X -> Y") %>%
#'          set_prior_distribution()
#'  estimand_1  <- estimand_distribution(model, query = "Y[X=1] - Y[X=0]")
#'  estimand_2  <- estimand_distribution(model, query = "Y[X=1] > Y[X=0]")

estimand_distribution <- function(model,
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
#' model <- make_model("X -> Y") %>%
#'            set_prior_distribution(n_draws = 10000)
#'
#' get_estimands(
#'       model,
#'       queries = list(ATE = "Y[X=1] - Y[X=0]",
#'                      Share_positive = "Y[X=1] > Y[X=0]"))
#'
get_estimands <- function(model,
																				 lambda = NULL,
																				 queries = list(NULL),
																				 subsets = list(TRUE),
																				 posteriors = list(FALSE),
																				 estimand_labels = NULL,
																				 stats = NULL){

	if(is.null(stats)) {if(!is.null(lambda)) {stats <- c(mean  = mean)} else {stats <- c(mean = mean, sd = sd)}}


	if(is.null(estimand_labels)) estimand_labels <- names(queries)

	f <- function(q, subset, posterior){
		v <- estimand_distribution(model, query = q, subset = subset, lambda = lambda, posterior = posterior, verbose = FALSE)
		sapply(stats, function(g) g(v))
	}

	out <- matrix(mapply(f, queries, subsets, posteriors), nrow = length(stats))
	rownames(out) <- paste(names(stats))
	if(!is.null(estimand_labels)) {colnames(out) <- estimand_labels} else {colnames(out) <- paste0("Q", 1:ncol(out))}
	data.frame(out)
}





