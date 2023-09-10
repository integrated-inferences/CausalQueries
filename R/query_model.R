#' Calculate query distribution
#'
#' Calculated distribution of a query from a prior or posterior distribution of parameters
#'
#'
#' @inheritParams CausalQueries_internal_inherit_params
#' @param parameters A vector of real numbers in [0,1].  A true parameter vector to be used instead of parameters attached to the model in case  \code{using} specifies \code{parameters}
#' @param using A character. Whether to use priors, posteriors or parameters
#' @param query A character. A query on potential outcomes such as "Y[X=1] - Y[X=0]"
#' @param join_by A character. The logical operator joining expanded types when \code{query} contains wildcard (\code{.}). Can take values \code{"&"} (logical AND) or \code{"|"} (logical OR). When restriction contains wildcard (\code{.}) and \code{join_by} is not specified, it defaults to \code{"|"}, otherwise it defaults to \code{NULL}.
#' @param given  A character. A quoted expression evaluates to logical statement. \code{given} allows the query to be conditioned on *observational* distribution. A value of TRUE is interpreted as no conditioning.
#' @param type_distribution A numeric vector. If provided saves calculation, otherwise calculated from model; may be based on prior or posterior
#' @param verbose Logical. Whether to print mean and standard deviation of the estimand on the console.
#' @param case_level Logical. If TRUE estimates the probability of the query for a case.
#' @return A vector of draws from the distribution of the potential outcomes specified in \code{query}
#' @importFrom stats sd weighted.mean
#' @export
#' @examples
#' model <- make_model("X -> Y") %>%
#'          set_parameters(c(.5, .5, .1, .2, .3, .4))
#'  \donttest{
#'  # simple  queries
#'  query_distribution(model, query = "(Y[X=1] > Y[X=0])")
#'
#'  # linear queries
#'  query_distribution(model, query = "(Y[X=1] - Y[X=0])")
#'
#'  # queries conditional on observables
#'  query_distribution(model, query = "(Y[X=1] > Y[X=0])", given = "X==1 & Y ==1")
#'
#'  # Linear query conditional on potential outcomes
#'  query_distribution(model, query = "(Y[X=1] - Y[X=0])", given = "Y[X=1]==0")
#'
#'  # Use join_by to amend query interpretation
#'  query_distribution(model, query = "(Y[X=.] == 1)", join_by = "&")
#'
#'  # Probability of causation query with verbose output
#'  query_distribution(model,
#'     query = "(Y[X=1] > Y[X=0])",
#'     given = "X==1 & Y==1",
#'     using = "priors",
#'     verbose = TRUE)  |> head()
#'
#'  # Case level probability of causation query with verbose output
#'  query_distribution(model,
#'     query = "(Y[X=1] > Y[X=0])",
#'     given = "X==1 & Y==1",
#'     case_level = TRUE,
#'     using = "priors",
#'     verbose = TRUE)
#'
#'  # Query posterior
#'  update_model(model, make_data(model, n = 3)) |>
#'  query_distribution(query = "(Y[X=1] - Y[X=0])", using = "posteriors") |>
#'  head()
#'
#'  # Case level queries provide the inference for a case, which is a scalar
#'  # The case level query *updates* on the given information
#'  # For instance, here we have a model for which we are quite sure that X causes Y but we do not
#'  # know whether it works through two positive effects or two negative effects
#'  # Thus we do not know if M=0 would suggest an effect or no effect
#'
#'  set.seed(1)
#'  model <-
#'    make_model("X -> M -> Y") |>
#'    update_model(data.frame(X = rep(0:1, 8), Y = rep(0:1, 8)), iter = 10000)
#'  Q <- "Y[X=1] > Y[X=0]"
#'  G <- "X==1 & Y==1 & M==1"
#'  QG <- "(Y[X=1] > Y[X=0]) & (X==1 & Y==1 & M==1)"
#'
#'  # In this case these are very different:
#'  query_distribution(model, Q, given = G, using = "posteriors") |> mean()
#'  query_distribution(model, Q, given = G, using = "posteriors", case_level = TRUE)
#'
#'  # These are equivalent:
#'  # 1. Case level query via function
#'  query_distribution(model, Q, given = G, using = "posteriors", case_level = TRUE)
#'
#'  # 2. Case level query by hand using Bayes
#'  mean(query_distribution(model, QG, using = "posteriors") )/
#'  mean(query_distribution(model, G, using = "posteriors"))
#' }
query_distribution <-
  function(model,
           query,
           given = TRUE,
           using  = "parameters",
           parameters = NULL,
           type_distribution = NULL,
           verbose = FALSE,
           join_by = "|",
           case_level = FALSE) {

  # forgive the user:
  if(using == "posterior") using <- "posteriors"
  if(using == "prior")     using <- "priors"
  if(given %in% c("TRUE", "All", "all", "ALL", "None", "none", "NONE", "")) given <- TRUE

  if(!(using %in% c("priors", "posteriors", "parameters"))) stop(
    "`using` should be one of `priors`, `posteriors`, or `parameters`")

  given_types <- given

  if(!is.logical(given)) given_types <- map_query_to_causal_type(model, given)$types

  if(all(!given_types)) {message("No units in given"); return() }


  # Evaluation of query on vector of causal types
  x <- (map_query_to_causal_type(model, query = query)$types)[given_types]

  # Parameters specified
  if(using =="parameters"){

    if(is.null(type_distribution)){
      if(is.null(parameters)) parameters <- get_parameters(model)
      type_distribution <- get_type_prob(model, parameters = parameters)}

    type_distribution <- type_distribution[given_types]

    # always case level if using = parameters
    # return(weighted.mean(x, type_distribution[given]))
    return(sum(x * type_distribution) /sum(type_distribution))

  }

  # slow step
  # set keep_transformed=TRUE in updated model to ensure access to type_distribution
  # adding P to model can also speed up slightly
  if(is.null(type_distribution))
    type_distribution <- get_type_prob_multiple(model, using = using, P = model$P)

  type_distribution <- matrix(type_distribution[given_types, ], ncol = ncol(type_distribution))

  # Subsetting implemented on type_distribution prior to take weighted mean
  # This gets the distribution of conditional values
  if(!case_level){
    #	  estimand <- apply(type_distribution[given,], 2, function(wt) weighted.mean(x[given], wt))
    estimand <- (x %*% type_distribution) / apply(type_distribution, 2, sum)
  }

  # This gets the expected of conditional values
  if(case_level){
    estimand <- mean(x %*% type_distribution) /mean( apply(type_distribution, 2, sum))
  }
  if(verbose) print(paste("mean = ", round(mean(estimand), 3), "; sd = ", round(sd(estimand),3)))


  # names(out) <- ifelse(is.logical(given), query, paste(query, " | ", given))

  estimand |> unlist() |> as.vector()

}


#' Generate estimands dataframe
#'
#' Calculated from a parameter vector, from a prior or from a posterior distribution.
#'
#' Queries can condition on observed or counterfactual quantities. Nested or "complex" counterfacttual queries of the form \code{Y[X=1, M[X=0]]} are allowed.
#'
#' @inheritParams CausalQueries_internal_inherit_params
#' @param queries A vector of characters. Query on potential outcomes such as "Y[X=1] - Y[X=0]".
#' @param given A character. A quoted expression that evaluates to a logical statement. Allows estimand to be conditioned on *observational* (or counterfactual) distribution.
#' @param using A character. Whether to use priors, posteriors or parameters.
#' @param stats Functions to be applied to estimand distribution. If NULL, defaults to mean, standard deviation, and 95\% confidence interval.
#' @param n_draws An integer. Number of draws.
#' @param expand_grid Logical. If \code{TRUE} then all combinations of provided lists are examined. If not then each list is cycled through separately. Defaults to FALSE.
#' @param case_level Logical. If TRUE estimates the probability of the query for a case.
#' @param query alias for queries
#' @return A \code{data.frame} with columns Query, Given and Using defined by corresponding input values. Further columns are generated as specified in \code{stats}.
#' @export
#' @examples
#' model <- make_model("X -> Y") %>% set_prior_distribution(n_draws = 10000)
#'
#' \donttest{
#' query_model(
#'   model,
#'   query = list(ATE = "Y[X=1] - Y[X=0]", Share_positive = "Y[X=1] > Y[X=0]"),
#'   using = c("parameters", "priors"),
#'   expand_grid = TRUE)
#'
#' query_model(
#'   model,
#'   query = list(ATE = "Y[X=1] - Y[X=0]", Share_positive = "Y[X=1] > Y[X=0]"),
#'   using = c("parameters", "priors"),
#'   expand_grid = FALSE)
#'
#' query_model(
#'   model,
#'   using = list( "parameters", "priors"),
#'   query = list(ATE = "Y[X=1] - Y[X=0]", Is_B = "Y[X=1] > Y[X=0]"),
#'   given = list(TRUE,  "Y==0 & X==1"),
#'   expand_grid = TRUE)
#'
#' # An example of a custom statistic: uncertainty of token causation
#' token_var <- function(x) mean(x)*(1-mean(x))
#' query_model(
#'   model,
#'   using = list( "parameters", "priors"),
#'   query = "Y[X=1] > Y[X=0]",
#'   stats = c(mean = mean, sd = sd, token_var = token_var))
#'}

query_model <-

  function(model,
           queries    = NULL,
           given      = NULL,
           using      = list("parameters"),
           parameters = NULL,
           stats      = NULL,
           n_draws    = 4000,
           expand_grid = FALSE,
           case_level = FALSE,
           query = NULL) {

  is_a_model(model)
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

  if(is.null(stats)) {if(!is.null(parameters)) {
    stats <- c(mean  = mean)
    } else {
    stats <- c(mean = mean,
               sd = sd,
               cred.low = function(x) stats::quantile(x, probs = 0.025),
               cred.high = function(x) stats::quantile(x, probs = 0.975)
               )
    }}


  #

  # Make complete vector of names, with imputation if needed
  if(is.null(names(queries)))  names(queries) <- paste("Q", 1:length(queries))
  for(j in 1:length(queries)) if(names(queries[j])=="") names(queries)[j] <- paste("Q", j)
  query_names <- names(queries)

  # Cross product of conditions to be examined
  if(expand_grid){
    query_names <- expand.grid(using, given, query_names, case_level, stringsAsFactors = FALSE)[,3]
    grid    <- expand.grid(using, given, queries, case_level, stringsAsFactors = FALSE)
    case_level <- grid[,4]
    queries <- grid[,3]
    given   <- grid[,2]
    using   <- grid[,1]}

  # Type distribution: Calculated once for speed
  using_used <- unique(unlist(using))

  dists <- lapply(using_used, function(j) {
    get_type_prob_multiple(model, using = j)
  }
  )
  names(dists) <- using_used

  # Function for mapply
  f <- function(query, given, using, case_level){

    v <- query_distribution(model,
                            query   = query,
                            given   = given,
                            using   = using,
                            case_level   = case_level,
                            type_distribution = dists[[using]], # select the right one by name
                            parameters = parameters,
                            verbose = FALSE)

    # for cases in which evaluation sought on impossible given
    if(is.null(v)) return(rep(NA, length(stats)))

    # return
    sapply(stats, function(g) g(v))

  }

  # Implementation
  out <- mapply(f, queries, given, using, case_level)

  ## Clean up: 'if' used here only because shape depends on length of stats
  if(length(stats)==1) out <- data.frame(mean = as.vector(out), stringsAsFactors = FALSE)
  if(length(stats)> 1) out <- data.frame(t(out), stringsAsFactors = FALSE)

  ## Clean up: mapply again for identifiers
  h    <- function(qname, given, using, case_level){ c(qname, paste(given), using, case_level)}
  cols <- data.frame(t(mapply(h, query_names, given, using, case_level)), stringsAsFactors = FALSE)

  ## Clean up: formatting
  out <- cbind(cols, out)
  names(out) <- c( "Query", "Given", "Using", "Case estimand", paste(names(stats)))
  out$Given[out$Given == "TRUE"] <- "-"
  rownames(out) <- NULL

  data.frame(out)

}

