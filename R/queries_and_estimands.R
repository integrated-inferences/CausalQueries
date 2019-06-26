#' Calculate estimand distribution
#'
#' Calculated from a prior or posterior distribution
#'
#'
#' @param model A  model
#' @param parameters A true parameter vector to be used instead of parameters attached to the model in case  `using` specifies `parameters`
#' @param using String indicating whether to use `priors`, `posteriors` or `parameters`
#' @param query A query on potential otcomes such as "Y[X=1] - Y[X=0]"
#' @param subset quoted expression evaluates to logical statement. subset allows estimand to be conditioned on *observational* distribution.
#' @param type_distribution if provided saves calculation, otherwise clculated from model; may be based on prior or posterior
#' @export
#' @examples
#' model <- make_model("X -> Y") %>%
#'          set_prior_distribution()
#'  estimand_distribution(model, query = "(Y[X=1] - Y[X=0])")
#'  estimand_distribution(model, query = "(Y[X=1] - Y[X=0])", subset = "X==1")
#'  estimand_distribution(model, query = "(Y[X=1] - Y[X=0])", subset = "Y[X=1]==1")
#'  estimand_distribution(model, query = "(Y[X=1] > Y[X=0])")
#'  estimand_distribution(model, query = "(Y[X=1] - Y[X=0])", using = "posteriors")
#'  estimand_distribution(model, query = "(Y[X=1] - Y[X=0])", using = "parameters")

estimand_distribution <- function(model,
															 query,
															 subset = TRUE,
															 using  = "priors",
															 parameters = NULL, # Use for example if true parameters known
															 type_distribution = NULL,
															 verbose = FALSE) {

  if(!(using %in% c("priors", "posteriors", "parameters"))) stop(
  	"`using` should be one of `priors`, `posteriors`, or `parameters`")

	# if(!is.logical(subset)) subset <- with(reveal_outcomes(model),
	#																			 eval(parse(text = subset)))

	if(!is.logical(subset)) subset <- get_types(model, subset)$types

	if(all(!subset)) {message("No units in subset"); return() }

  if(using == "parameters" & is.null(model$parameters) & is.null(parameters)) stop("please provide parameters")


		# Evaluation of query on vector of causal types
	x <- (get_types(model, query = query)$types)[subset]


	if(using =="parameters"){
		if(is.null(parameters)) parameters <- model$parameters

		type_distribution <- draw_type_prob(model, parameters = parameters)[subset]
		return(weighted.mean(x, type_distribution))
		}

	if(is.null(type_distribution)) type_distribution <-
		draw_type_prob_multiple(model, using = using)[subset,]
  	estimand <- apply(type_distribution, 2, function(wt) weighted.mean(x, wt))

  if(verbose) print(paste("mean = ", round(mean(estimand), 3), "; sd = ", round(sd(estimand),3)))

  estimand
}


#' Generate estimands dataframe
#'
#' Calculated from a parameter vector, from a prior or from a posterior distribution
#'
#'
#' @param model A  model
#' @param parameters A  true parameter vector, if available
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
#'                      Share_positive = "Y[X=1] > Y[X=0]"),
#'       using = c("parameters", "priors"))
#'
#' get_estimands(
#'       model,
#'       queries = list(ATE = "Y[X=1] - Y[X=0]",
#'                      Share_positive = "Y[X=1] > Y[X=0]"),
#'       using = "priors")
#'
#' get_estimands(
#'       model,
#'       queries = list(ATE = "Y[X=1] - Y[X=0]"),
#'       using = list("priors", "parameters"),
#'       digits = 3)
#'
#' get_estimands(
#'       model,
#'       using = "priors",
#'       queries = list(Is_B = "Y[X=1] > Y[X=0]"),
#'       subsets = list(TRUE, "Y==1 & X==1", "Y==0 & X==1"),
#'       digits = 3)
#'
#' get_estimands(
#'       model,
#'       using = "parameters",
#'       queries = list(Is_B = "Y[X=1] > Y[X=0]"),
#'       subsets = list(TRUE, "Y[X=1]==1", "Y==1"),
#'       digits = 3)


get_estimands <- function(model,
													parameters = NULL,
													queries = list(NULL),
													subsets = list(TRUE),
													using   = list(FALSE),
													stats = NULL,
													digits = 3,
													n_draws = 4000){

	if(("priors" %in% unlist(using)) & is.null(model$prior_distribution)){
		model <- set_prior_distribution(model, n_draws = n_draws)}

	if(is.null(stats)) {if(!is.null(parameters)) {stats <- c(mean  = mean)} else {stats <- c(mean = mean, sd = sd)}}

	if(!is.null(names(queries))) query_names <- names(queries)
	if(is.null(names(queries)))  query_names <- "not named"

	# Function for mapply
	f <- function(query, subset, using){
		v <- estimand_distribution(model,
															 query = query,
															 subset = subset,
															 parameters = parameters,
															 using = using,
															 verbose = FALSE)
		# FLAG: if needed for cases where evaluation sough on impossible subsets
		if(is.null(v)) {rep(NA, length(stats))} else {c(round(sapply(stats, function(g) g(v)), digits))}
	}
	 # FLAG: if needed because shape depends on length of stats -- must be better way

	if(length(stats)==1) out <- data.frame(mean = as.vector(mapply(f, queries, subsets, using)), stringsAsFactors = FALSE)
	if(length(stats)> 1) out <- data.frame(t(mapply(f, queries, subsets, using)), stringsAsFactors = FALSE)

	## mapply again for identifiers
	h <- function(qname, subset, using){ c(qname, paste(subset), using)}
	cols <- data.frame(t(mapply(h, query_names, subsets, using)), stringsAsFactors = FALSE)

	out <- cbind(cols, out)

	# Clean up
	names(out) <- c( "Query", "Subset", "Using", paste(names(stats)))
	out$Subset[out$Subset == "TRUE"] <- "All"
	rownames(out) <- NULL
	data.frame(out)
}
