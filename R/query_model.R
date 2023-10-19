#' Calculate query distribution
#'
#' Calculated distribution of a query from a prior or posterior distribution of parameters
#'
#'
#' @inheritParams CausalQueries_internal_inherit_params
#' @param parameters A vector or list of vectors of real numbers in [0,1].  A true parameter vector to be used instead of parameters attached to the model in case  \code{using} specifies \code{parameters}
#' @param using A character. Whether to use priors, posteriors or parameters
#' @param queries A character vector or list of character vectors specifying queries on potential outcomes such as "Y[X=1] - Y[X=0]"
#' @param join_by A character. The logical operator joining expanded types when \code{query} contains wildcard (\code{.}). Can take values \code{"&"} (logical AND) or \code{"|"} (logical OR). When restriction contains wildcard (\code{.}) and \code{join_by} is not specified, it defaults to \code{"|"}, otherwise it defaults to \code{NULL}.
#' @param given  A character vector specifying givens for each query. A given is a quoted expression that evaluates to logical statement. \code{given} allows the query to be conditioned on *observational* distribution. A value of TRUE is interpreted as no conditioning.
#' @param n_draws An integer. Number of draws.
#' @param type_distribution A numeric vector or list of numeric vectors. If provided saves calculation, otherwise calculated from model; may be based on prior or posterior
#' @param case_level Logical. If TRUE estimates the probability of the query for a case.
#' @param query alias for queries
#' @return A \code{DataFrame} where columns contain draws from the distribution of the potential outcomes specified in \code{query}
#' @importFrom stats sd weighted.mean
#' @export
#' @examples
#' model <- make_model("X -> Y") %>%
#'          set_parameters(c(.5, .5, .1, .2, .3, .4))
#'  \donttest{
#'  # simple  queries
#'  query_distribution(model, query = "(Y[X=1] > Y[X=0])", using = "priors") |>
#'    head()
#'
#'  # multiple  queries
#'  query_distribution(model,
#'    query = list("(Y[X=1] > Y[X=0])", "(Y[X=1] < Y[X=0])"), using = "priors")|>
#'    head()
#'
#'  # multiple queries and givens
#'  query_distribution(model,
#'    query = list("(Y[X=1] > Y[X=0])", "(Y[X=1] < Y[X=0])"),
#'    given = list("Y==1", "(Y[X=1] <= Y[X=0])"),
#'    using = "priors")|>
#'    head()
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
#'  # Probability of causation query
#'  query_distribution(model,
#'     query = "(Y[X=1] > Y[X=0])",
#'     given = "X==1 & Y==1",
#'     using = "priors")  |> head()
#'
#'  # Case level probability of causation query
#'  query_distribution(model,
#'     query = "(Y[X=1] > Y[X=0])",
#'     given = "X==1 & Y==1",
#'     case_level = TRUE,
#'     using = "priors")
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
#'
#'  Q <- "Y[X=1] > Y[X=0]"
#'  G <- "X==1 & Y==1 & M==1"
#'  QG <- "(Y[X=1] > Y[X=0]) & (X==1 & Y==1 & M==1)"
#'
#'  # In this case these are very different:
#'  query_distribution(model, Q, given = G, using = "posteriors")[[1]] |> mean()
#'  query_distribution(model, Q, given = G, using = "posteriors",
#'    case_level = TRUE)
#'
#'  # These are equivalent:
#'  # 1. Case level query via function
#'  query_distribution(model, Q, given = G,
#'     using = "posteriors", case_level = TRUE)
#'
#'  # 2. Case level query by hand using Bayes
#'  distribution <- query_distribution(
#'     model, list(QG = QG, G = G), using = "posteriors")
#'
#'  mean(distribution$QG)/mean(distribution$G)
#' }
query_distribution <- function(model,
                               queries,
                               given = NULL,
                               using  = "parameters",
                               parameters = NULL,
                               n_draws = 4000,
                               type_distribution = NULL,
                               join_by = "|",
                               case_level = FALSE,
                               query = NULL) {
  ## check arguments
  if(is(model, "causal_model")) {
    model <- list(model)
  }

  if(length(model) > 1) {
    stop("Please specify a single causal model in the `model` argument. You can pass a `causal_model` object directly or wrap it in a `list`.")
  }

  if(!is.null(query)) {
    queries <- query
  }

  if((!is.null(parameters)) && (!is.list(parameters))) {
    parameters <- list(parameters)
  }

  if((!is.null(parameters)) && (length(parameters) > 1)) {
    stop("Please only specify one set of parameters for your model.")
  }

  if((!is.null(type_distribution)) && (!is.list(type_distribution))) {
    type_distributions <- list(type_distribution)
  }

  if((!is.null(type_distribution)) && (length(type_distribution) > 1)) {
    stop("Please only specify one type_distribution for your model.")
  }

  args_checked <- check_args(model = model,
                             using = unlist(using),
                             given = unlist(given),
                             queries = queries,
                             case_level = case_level,
                             fun = "query_distribution")

  given <- args_checked$given
  using <- args_checked$using

  ## generate required data structures
  # generate model names
  model_names <- "model_1"
  names(model) <- model_names

  if(!is.null(parameters)) {
    names(parameters) <- model_names
  }

  if(!is.null(type_distribution)) {
    names(type_distributions) <- model_names
  }

  # generate outcome realisations
  realisations <- lapply(model, function(m) realise_outcomes(model = m))
  names(realisations) <- model_names

  # prevent bugs from query helpers
  given <- sapply(given, as.character)
  queries <- sapply(queries, as.character)

  jobs <- lapply(model_names, function(m) {
    data.frame(
      model_names = m,
      using = unlist(using),
      given = unlist(given),
      queries = unlist(queries),
      case_level = unlist(case_level),
      stringsAsFactors = FALSE)
  }) |>
    dplyr::bind_rows()

  # only generate necessary data structures for unique subsets of jobs
  # handle givens
  given_types <- queries_to_types(jobs = jobs,
                                  model = model,
                                  query_col = "given",
                                  realisations = realisations)
  # handle queries
  query_types <- queries_to_types(jobs = jobs,
                                  model = model,
                                  query_col = "queries",
                                  realisations = realisations)
  # handle type distributions
  type_distributions <- get_type_distributions(jobs = jobs,
                                               model = model,
                                               n_draws = n_draws,
                                               parameters = parameters)

  ## get estimands
  estimands <- get_estimands(jobs = jobs,
                             given_types = given_types,
                             query_types = query_types,
                             type_distributions = type_distributions) |>
    as.data.frame()

  ## prepare output
  if(!is.null(names(queries))) {
    colnames(estimands) <- make.unique(names(queries), sep = "_")
  } else {
    query_names <- jobs$queries
    given_names <- jobs$given
    given_names <- sapply(given_names, function(g) {
      if(g == "ALL") {
        gn <- ""
      } else {
        gn <- paste(" | ", g, sep = "")
      }
      return(gn)
    })
    colnames(estimands) <- make.unique(paste(query_names, given_names, sep = ""), sep = "_")
  }

  return(estimands)
}


#' Generate estimands dataframe
#'
#' Calculated from a parameter vector, from a prior or from a posterior distribution.
#'
#' Queries can condition on observed or counterfactual quantities. Nested or "complex" counterfactual queries of the form \code{Y[X=1, M[X=0]]} are allowed.
#'
#' @inheritParams CausalQueries_internal_inherit_params
#' @param queries A vector of strings or list of strings specifying queries on potential outcomes such as "Y[X=1] - Y[X=0]".
#' @param given A vector or list of strings specifying givens. A given is a quoted expression that evaluates to a logical statement. Allows estimand to be conditioned on *observational* (or counterfactual) distribution.
#' @param using A vector or list of strings. Whether to use priors, posteriors or parameters.
#' @param stats Functions to be applied to estimand distribution. If NULL, defaults to mean, standard deviation, and 95\% confidence interval.
#' @param n_draws An integer. Number of draws.
#' @param expand_grid Logical. If \code{TRUE} then all combinations of provided lists are examined. If not then each list is cycled through separately. Defaults to FALSE.
#' @param case_level Logical. If TRUE estimates the probability of the query for a case.
#' @param query alias for queries
#' @return A \code{DataFrame} with columns Model, Query, Given and Using defined by corresponding input values. Further columns are generated as specified in \code{stats}.
#' @export
#' @examples
#' model <- make_model("X -> Y") %>% set_prior_distribution(n_draws = 10000)
#'
#' \donttest{
#'
#' # `expand_grid= TRUE` requests the Cartesian product of arguments
#'
#' models <- list(
#'  M1 = make_model("X -> Y"),
#'  M2 = make_model("X -> Y") |> set_restrictions("Y[X=1] < Y[X=0]")
#'  )
#'
#'
#' query_model(
#'   models,
#'   query = list(ATE = "Y[X=1] - Y[X=0]", Share_positive = "Y[X=1] > Y[X=0]"),
#'   given = c(TRUE,  "Y==1 & X==1"),
#'   using = c("parameters", "priors"),
#'   expand_grid = FALSE)
#'
#' query_model(
#'   models,
#'   query = list(ATE = "Y[X=1] - Y[X=0]", Share_positive = "Y[X=1] > Y[X=0]"),
#'   given = c(TRUE,  "Y==1 & X==1"),
#'   using = c("parameters", "priors"),
#'   expand_grid = TRUE)
#'
#' # An example of a custom statistic: uncertainty of token causation
#' f <- function(x) mean(x)*(1-mean(x))
#'
#' query_model(
#'   model,
#'   using = list( "parameters", "priors"),
#'   query = "Y[X=1] > Y[X=0]",
#'   stats = c(mean = mean, sd = sd, token_variance = f))
#'}

query_model <- function(model,
                        queries = NULL,
                        given = NULL,
                        using = list("parameters"),
                        parameters = NULL,
                        stats = NULL,
                        n_draws = 4000,
                        expand_grid = FALSE,
                        case_level = FALSE,
                        query = NULL) {

  # if single model passed to function place it in a list
  if(is(model, "causal_model")) {
    model <- list(model)
  }

  ## check arguments
  if(!is.null(query) & !is.null(queries)) {
    stop("Please provide either queries or query.")
  }

  if(!is.null(query)) {
    queries <- query
  }

  if((!is.null(parameters)) && (!is.list(parameters))) {
    stop("Please specify parameters as a list of parameter vectors.")
  }

  if((!is.null(parameters)) && (length(model) != length(parameters))) {
    stop("Please specify parameters for each model.")
  }

  # check that parameters are specified for each model + named
  args_checked <- check_args(model = model,
                             using = unlist(using),
                             given = unlist(given),
                             queries = queries,
                             case_level = case_level,
                             fun = "query_model")

  given <- args_checked$given
  using <- args_checked$using

  ## generate required data structures
  # generate model names
  if(!is.null(names(model))) {
    model_names <- names(model)
  } else {
    model_names <- paste("model", 1:length(model), sep = "_")
    names(model) <- model_names
  }

  if(!is.null(parameters)) {
    names(parameters) <- model_names
  }

  # query names
  query_names <- names(queries)
  if(!is.null(query_names)) {
    names(query_names) <- unlist(queries)
  }

  # realise_outcomes
  realisations <- lapply(model, function(m) realise_outcomes(model = m))
  names(realisations) <- model_names

  # prevent bugs from query helpers
  given <- sapply(given, as.character)
  queries <- sapply(queries, as.character)

  # create jobs
  if(expand_grid) {
    jobs <- expand.grid(model_names,
                        unname(unlist(using)),
                        unname(unlist(given)),
                        unname(unlist(queries)),
                        unname(unlist(case_level)),
                        stringsAsFactors = FALSE)
    names(jobs) <- c("model_names","using","given","queries","case_level")
  } else {
    jobs <- lapply(model_names, function(m) {
      data.frame(
        model_names = m,
        using = unname(unlist(using)),
        given = unname(unlist(given)),
        queries = unname(unlist(queries)),
        case_level = unname(unlist(case_level)),
        stringsAsFactors = FALSE)
    }) |>
      dplyr::bind_rows()
  }

  # only generate necessary data structures for unique subsets of jobs
  # handle givens
  given_types <- queries_to_types(jobs = jobs,
                                  model = model,
                                  query_col = "given",
                                  realisations = realisations)
  # handle queries
  query_types <- queries_to_types(jobs = jobs,
                                  model = model,
                                  query_col = "queries",
                                  realisations = realisations)
  # handle type distributions
  type_distributions <- get_type_distributions(jobs = jobs,
                                               model = model,
                                               n_draws = n_draws,
                                               parameters = parameters)

  ## get estimands
  estimands <- get_estimands(jobs = jobs,
                             given_types = given_types,
                             query_types = query_types,
                             type_distributions = type_distributions)

  # compute statistics
  if(is.null(stats)) {
    if(!is.null(parameters)) {
      stats <- c(mean = mean)
    } else {
      stats <- c(mean = mean,
                 sd = sd,
                 cred.low = function(x) stats::quantile(x, probs = 0.025, na.rm = TRUE),
                 cred.high = function(x) stats::quantile(x, probs = 0.975, na.rm = TRUE))
    }
  }

  estimands <- lapply(estimands, function(e) sapply(stats, function(s) s(e)) |> t())
  estimands <- as.data.frame(do.call(rbind, estimands))

  ## prepare output
  query_id <- jobs |>
    dplyr::select(model_names, queries, given, using, case_level) |>
    dplyr::mutate(given = ifelse(given == "ALL","-",given))

  colnames(query_id) <- c("model","query","given","using","case_level")

  if(!is.null(query_names)) {
    query_id$query <- query_names[query_id$query]
  }

  estimands <- cbind(query_id, estimands)

  if(length(model) == 1) {
    estimands <- estimands[,colnames(estimands) != "model"]
  }

  return(estimands)
}



#' helper to check arguments
#'
#' @param model passed from parent function
#' @param using passed from parent function
#' @param given passed from parent function
#' @param queries passed from parent function
#' @param fun string specifying the name of the parent function
#' @return list of altered arguments
#' @keywords internal

check_args <- function(model, using, given, queries, case_level, fun) {
  lapply(model, function(m) is_a_model(m))

  using[using == "posterior"] <- "posteriors"
  using[using == "prior"] <- "priors"

  if((fun == "query_distribution") && is.null(given)) {
    given <- rep("ALL", length(queries))
  }

  if((fun == "query_model") && is.null(given)) {
    given <- "ALL"
  }

  if(is.logical(given)) {
    given <- as.character(given)
  }

  if(!is.null(given) && !is.character(given)) {
    stop("`given` must be a vector of strings specifying given statements or '', 'All', 'ALL', 'all', 'None', 'none', 'NONE' or 'TRUE' for no givens.")
  }

  if((fun == "query_distribution") && (!is.null(given)) && (length(given) != length(queries))) {
    stop("You must specify a given for each query. Use '', 'All', 'ALL', 'all', 'None', 'none', 'NONE' or 'TRUE' to indicate no given.")
  }

  if(any(!using %in% c("priors", "posteriors", "parameters"))) {
    stop("`using` may only take values: `priors`, `posteriors`, or `parameters`")
  }

  given[given %in% c('', 'All', 'ALL', 'all', 'None', 'none', 'NONE', 'TRUE')] <- "ALL"

  if((fun == "query_distribution") && (length(using) > 1)) {
    stop("You can only specify a single value for the `using` argument.")
  }

  if((fun == "query_distribution") && (length(case_level) > 1)) {
    stop("You can only specify a single value for the `case_level` argument.")
  }

  return(list(given = given, using = using))
}


#' helper to get types from queries
#'
#' @param jobs \code{DataFrame} of argument combinations
#' @param model a list of models
#' @param query_col string specifying the name of the column in jobs holding queries to be evaluated
#' @param realisations list of \code{DataFrame} outputs from calls to \code{realise_outcomes}
#' @return jobs \code{DataFrame} with a nested column of \code{map_query_to_nodal_type} outputs
#' @keywords internal

queries_to_types <- function(jobs, model, query_col, realisations) {
  unique_jobs <- dplyr::distinct(jobs, (!!as.name("model_names")), (!!as.name(query_col)))
  types <- vector(mode = "list", length = nrow(unique_jobs))

  for(i in 1:nrow(unique_jobs)) {
    model_i <- unique_jobs[i, "model_names"]

    if((query_col == "given") && (unique_jobs[i,query_col] == "ALL")) {
      types[[i]] <- TRUE
    } else {
      types[[i]] <- map_query_to_causal_type(model = model[[model_i]],
                                             query = unique_jobs[i, query_col],
                                             eval_var = realisations[[model_i]])$types
    }
  }
  unique_jobs$type_vec <- types
  return(unique_jobs)
}

#' helper to get type distributions
#'
#' @param jobs \code{DataFrame} of argument combinations
#' @param model a list of models
#' @param n_draws integer specifying number of draws from prior distribution
#' @param parameters optional list of parameter vectors
#' @return jobs \code{DataFrame} with a nested column of type distributions
#' @keywords internal

get_type_distributions <- function(jobs, model, n_draws, parameters = NULL) {
  unique_jobs <- dplyr::distinct(jobs, (!!as.name("model_names")), (!!as.name("using")))
  distributions <- vector(mode = "list", length = nrow(unique_jobs))

  if(is.null(parameters)) {
    parameters <- list()
  }

  for(i in 1:nrow(unique_jobs)) {
    model_i <- unique_jobs[i, "model_names"]
    using_i <- unique_jobs[i, "using"]

    if((using_i == "parameters") && is.null(parameters[[model_i]])) {
      parameters[[model_i]] <- get_parameters(model[[model_i]])
    }

    if((using_i == "priors") && is.null(model[[model_i]]$prior_distribution)) {
      model[[model_i]] <- set_prior_distribution(model[[model_i]], n_draws = n_draws)
    }

    if(using_i == "parameters") {
      distributions[[i]] <- get_type_prob(model = model[[model_i]], parameters = parameters[[model_i]])
    } else {
      distributions[[i]] <- get_type_prob_multiple(model = model[[model_i]], using = using_i, P = model[[model_i]]$P)
    }
  }
  unique_jobs$type_distribution <- distributions
  return(unique_jobs)
}

#' helper to get estimands
#'
#' @param jobs \code{DataFrame} of argument combinations
#' @param given_types output from \code{queries_to_types}
#' @param query_types output from \code{queries_to_types}
#' @param type_distributions output from \code{get_type_distributions}
#' @return a list of estimands
#' @keywords internal

get_estimands <- function(jobs, given_types, query_types, type_distributions) {
  estimands <- vector(mode = "list", length = nrow(jobs))

  for(i in 1:nrow(jobs)) {
    model_name_i <- jobs[i,"model_names"]
    using_i <- jobs[i, "using"]
    given_i <- jobs[i, "given"]
    queries_i <- jobs[i, "queries"]
    case_level_i <- jobs[i, "case_level"]

    x <- query_types[(query_types$model_names == model_name_i & query_types$queries == queries_i), "type_vec"][[1]]
    given <- given_types[(given_types$model_names == model_name_i & given_types$given == given_i), "type_vec"][[1]]
    type_distribution <- type_distributions[(type_distributions$model_names == model_name_i & type_distributions$using == using_i),"type_distribution"][[1]]
    x <- x[given]

    if(all(!given)) {
      estimand <- NA
      message("No units given. `NA` estimand.")
    } else {
      # using parameters
      if(using_i == "parameters") {
        # always case level when using parameters
        estimand <- sum(x * type_distribution[given]) / sum(type_distribution[given])
      }
      # using priors or posteriors
      if(using_i != "parameters") {
        # population level
        if(!case_level_i) {
          estimand <- (x %*% type_distribution[given,, drop = FALSE]) / apply(type_distribution[given,, drop = FALSE], 2, sum)
        }
        # case level
        if(case_level_i) {
          estimand <- mean(x %*% type_distribution[given,, drop = FALSE]) / mean(apply(type_distribution[given,, drop = FALSE], 2, sum))
        }
      }
    }
    estimands[[i]] <- as.vector(unlist(estimand))
  }
  return(estimands)
}

