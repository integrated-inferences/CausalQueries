#' Calculate query distribution
#'
#' Calculated distribution of a query from a prior or posterior distribution of parameters
#'
#'
#' @inheritParams CausalQueries_internal_inherit_params
#' @param parameters A vector of real numbers in [0,1].  A true parameter vector to be used instead of parameters attached to the model in case  `using` specifies `parameters`
#' @param using A character. Whether to use `priors`, `posteriors` or `parameters`
#' @param query A character. A query on potential outcomes such as "Y[X=1] - Y[X=0]"
#' @param join_by A character. The logical operator joining expanded types when \code{causal_type_restrict} contains wildcard (\code{.}). Can take values \code{"&"} (logical AND) or \code{"|"} (logical OR). When restriction contains wildcard (\code{.}) and \code{join_by} is not specified, it defaults to \code{"|"}, otherwise it defaults to \code{NULL}.
#' @param given  A character. A quoted expression evaluates to logical statement. given allows estimand to be conditioned on *observational* distribution.
#' @param type_distribution A numeric vector. If provided saves calculation, otherwise calculated from model; may be based on prior or posterior
#' @param verbose Logical. Whether to print mean and standard deviation of the estimand on the consule.
#' @importFrom stats sd weighted.mean
#' @export
#' @examples
#' model <- make_model("X -> Y") %>%
#'          set_prior_distribution()
#'
#'  distribution <- query_distribution(model, query = "(Y[X=1] - Y[X=0])")
#'  distribution <- query_distribution(model, query = "(Y[X=1] - Y[X=0])", given = "X==1")
#'  distribution <- query_distribution(model, query = "(Y[X=1] - Y[X=0])", given = "Y[X=1]==1")
#'  distribution <- query_distribution(model, query = "(Y[X=1] > Y[X=0])")
#'  distribution <- query_distribution(model, query = "(Y[X=.] == 1)", join_by = "&")
#'  distribution <- query_distribution(model, query = "(Y[X=1] - Y[X=0])", using = "parameters")
#' \dontrun{
#'  df    <- simulate_data(model, n = 3)
#'  updated_model <- CausalQueries(model, df)
#'  query_distribution( updated_model , query = "(Y[X=1] - Y[X=0])", using = "posteriors")
#' }
query_distribution <- function(model,
															 query,
															 given = TRUE,
															 using  = "priors",
															 parameters = NULL, # Use for example if true parameters known
															 type_distribution = NULL,
															 verbose = FALSE,
															 join_by = "|") {

	# forgive the user:
	if(using == "posterior") using <- "posteriors"
	if(using == "prior")     using <- "priors"
	if(given %in% c("TRUE", "All", "all", "ALL", "None", "none", "NONE")) given <- TRUE

  if(!(using %in% c("priors", "posteriors", "parameters"))) stop(
  	"`using` should be one of `priors`, `posteriors`, or `parameters`")

	if(!is.logical(given)) given <- map_query_to_causal_type(model, given)$types

	if(all(!given)) {message("No units in given"); return() }


		# Evaluation of query on vector of causal types
	x <- (map_query_to_causal_type(model, query = query)$types)[given]

	# Parameters specified
	if(using =="parameters"){

		if(is.null(type_distribution)){
			if(is.null(parameters)) parameters <- get_parameters(model)
			type_distribution <- get_type_prob(model, parameters = parameters)}

		return(weighted.mean(x, type_distribution[given]))
		}

	if(is.null(type_distribution)) type_distribution <- get_type_prob_multiple(model, using = using)

	# Magic: Subsetting implemented on type_distribution prior to taking weighted mean
  estimand <- apply(type_distribution[given,], 2, function(wt) weighted.mean(x, wt))

  if(verbose) print(paste("mean = ", round(mean(estimand), 3), "; sd = ", round(sd(estimand),3)))

  estimand
}


#' Generate estimands dataframe
#'
#' Calculated from a parameter vector, from a prior or from a posterior distribution
#'
#' @inheritParams CausalQueries_internal_inherit_params
#'
#' @param queries A vector of characters. Query on potential outcomes such as "Y[X=1] - Y[X=0]".
#' @param given A character. A quoted expression that evaluates to a logical statement. Allows estimand to be conditioned on *observational* (or counterfactual) distribution.
#' @param using A character. Whether to use `priors`, `posteriors` or `parameters`.
#' @param stats Functions to be applied to estimand distribution. If `NULL`, defaults to mean and standard deviation.
#' @param digits An integer. Decimal digits in output table.
#' @param n_draws An integer. Number of draws.
#' @param expand_grid Logical. If \code{TRUE} then all combinations of provided lists are examined. If not then each list is cycled through separately. Defaults to `FALSE`.
#' @param query alias for queries
#' @export
#' @examples
#' model <- make_model("X -> Y") %>% set_prior_distribution(n_draws = 10000)
#'
#' estimands_df <-query_model(
#'                model,
#'                query = list(ATE = "Y[X=1] - Y[X=0]", Share_positive = "Y[X=1] > Y[X=0]"),
#'                using = c("parameters", "priors"),
#'                expand_grid = TRUE)
#'
#' estimands_df <-query_model(
#'                model,
#'                query = list(ATE = "Y[X=1] - Y[X=0]", Share_positive = "Y[X=1] > Y[X=0]"),
#'                using = c("parameters", "priors"),
#'                expand_grid = FALSE)
#'
#' estimands_df <- query_model(
#'                 model,
#'                 using = list( "parameters", "priors"),
#'                 query = list(ATE = "Y[X=1] - Y[X=0]", Is_B = "Y[X=1] > Y[X=0]"),
#'                 given = list(TRUE,  "Y==0 & X==1"),
#'                 expand_grid = TRUE,
#'                 digits = 3)
#'
#' # An example: a stat representing uncertainty of token causation
#' token_var <- function(x) mean(x)*(1-mean(x))
#' estimands_df <- query_model(
#'                 model,
#'                 using = list( "parameters", "priors"),
#'                 query = "Y[X=1] > Y[X=0]",
#'                 stats = c(mean = mean, sd = sd, token_var = token_var))
#'

query_model <- function(model,
												queries    = NULL,
												given      = NULL,
												using      = list("priors"),
												parameters = NULL,
												stats      = NULL,
												digits     = 3,
												n_draws    = 4000,
												expand_grid = FALSE,
												query = NULL){

	if(is.null(query) & is.null(queries))  stop("No query provided.")
	if(!is.null(query) & !is.null(queries))  stop("Please provide either queries or query.")

	# Forgive user
	if(!is.null(query))   queries <- query
	if(is.null(given))  given <- TRUE

  # If parameters provided, add these to model
	if(!is.null(parameters)) model <- set_parameters(model, parameters)

	# Housekeeping
	if(("priors" %in% unlist(using)) & is.null(model$prior_distribution)){
		model <- set_prior_distribution(model, n_draws = n_draws)}

	if(all(using == "parameters") & is.null(stats)) stats <- c(mean = mean)
	if(is.null(stats)) {if(!is.null(parameters)) {stats <- c(mean  = mean)} else {stats <- c(mean = mean, sd = sd)}}

	# Make complete vector of names, with imputation if needed
	if(is.null(names(queries)))  names(queries) <- paste("Q", 1:length(queries))
	for(j in 1:length(queries)) if(names(queries[j])=="") names(queries)[j] <- paste("Q", j)
	query_names <- names(queries)

	# Cross product of conditions to be examined
	if(expand_grid){
		query_names <- expand.grid(using, given, query_names, stringsAsFactors = FALSE)[,3]
		grid <- expand.grid(using, given, queries, stringsAsFactors = FALSE)
		queries <- grid[,3]
		given <- grid[,2]
		using   <- grid[,1]}

	# Type distribution: Calculated once for speed
  using_used <- unique(unlist(using))

	dists <- lapply(using_used, function(j) {
		param_dist <- get_param_dist(model, j)  # parameter distribution extracted just once
		get_type_prob_multiple(model, using = j, param_dist = param_dist)}
		)
  names(dists) <- using_used

	# Function for mapply
	f <- function(query, given, using){

		v <- query_distribution(model,
														query   = query,
														given   = given,
														using   = using,
														type_distribution = dists[[using]], # select the right one by name
														parameters = parameters,
														verbose = FALSE)

		# for cases in which evaluation sought on impossible given
		if(is.null(v)) return(rep(NA, length(stats)))

		# return
		round(sapply(stats, function(g) g(v)), digits)

	}

  # Implementation
	out <- mapply(f, queries, given, using)

	## Clean up: 'if' used here only because shape depends on length of stats
	if(length(stats)==1) out <- data.frame(mean = as.vector(out), stringsAsFactors = FALSE)
	if(length(stats)> 1) out <- data.frame(t(out), stringsAsFactors = FALSE)

	## Clean up: mapply again for identifiers
	h    <- function(qname, given, using){ c(qname, paste(given), using)}
	cols <- data.frame(t(mapply(h, query_names, given, using)), stringsAsFactors = FALSE)

	## Clean up: formatting
	out <- cbind(cols, out)
	names(out) <- c( "Query", "Given", "Using", paste(names(stats)))
	out$Given[out$Given == "TRUE"] <- "-"
	rownames(out) <- NULL

	data.frame(out)

}
