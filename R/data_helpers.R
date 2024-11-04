#' Data helpers
#'
#' Various helpers to simulate data and to manipulate data types between compact and long forms.
#' @name data_helpers
NULL
#> NULL



#' Make compact data with data strategies
#'
#'\code{collapse_data} can be used to convert long form data to compact form data,
#'
#' @rdname data_helpers
#' @inheritParams CausalQueries_internal_inherit_params
#'
#' @param drop_NA Logical. Whether to exclude strategy families that contain
#'   no observed data. Exceptionally if no data is provided, minimal data on
#'   data on first node is returned. Defaults to `TRUE`
#' @param drop_family Logical. Whether to remove column \code{strategy} from
#'   the output. Defaults to `FALSE`.
#' @param summary Logical. Whether to return summary of the data. See details.
#'   Defaults to `FALSE`.
#' @export
#'
#' @importFrom dplyr left_join
#' @importFrom dplyr filter mutate
#'
#' @return A vector of data events
#'
#' If \code{summary = TRUE} `collapse_data` returns a list containing the
#'   following components:
#' \item{data_events}{A compact data.frame of event types and strategies.}
#'    \item{observed_events}{A vector of character strings specifying the events
#'      observed in the data}
#'    \item{unobserved_events}{A vector of character strings specifying the
#'      events not observed in the data}
#' @examples
#'\donttest{
#'
#' model <- make_model('X -> Y')
#'
#' df <- data.frame(X = c(0,1,NA), Y = c(0,0,1))
#'
#' df |> collapse_data(model)
#'
#' # Illustrating options
#'
#' df |> collapse_data(model, drop_NA = FALSE)
#'
#' df |> collapse_data(model, drop_family = TRUE)
#'
#' df |> collapse_data(model, summary = TRUE)
#'
#' # Appropriate behavior given restricted models
#'
#' model <- make_model('X -> Y') |>
#'   set_restrictions('X[]==1')
#' df <- make_data(model, n = 10)
#' df[1,1] <- ''
#' df |> collapse_data(model)
#'
#' df <- data.frame(X = 0:1)
#' df |> collapse_data(model)
#'
#' }
#'


collapse_data <- function(data,
                          model,
                          drop_NA = TRUE,
                          drop_family = FALSE,
                          summary = FALSE) {

    # Add missing nodes and order correctly
    nodes <- model$nodes
    if (any(!(nodes %in% names(data)))){
      data[nodes[!(nodes %in% names(data))]] <- NA
    }

    data <- data[, nodes, drop = FALSE]

    if (nrow(data) == 0 | all(is.na(data))) {
        data_events <- minimal_event_data(model)
        drop_NA <- FALSE

    } else {

        data_families <- get_data_families(model)[, c("event", "strategy")]
        data_type <- data_type_names(model, data)

        # Inconsistent data
        if (!all(unique(data_type) %in% data_families$event))
          message(paste0(
            unique(data_type)[!(unique(data_type) %in% data_families$event)],
            " data is inconsistent with model and ignored")
            )

        # Collapse
        data_events <- data.frame(table(data_type), stringsAsFactors = FALSE)
        colnames(data_events) <- c("event", "count")
        data_events$event <- as.character(data_events$event)

        # Merge in families
        data_events <- left_join(data_families, data_events, by = "event") |>
          mutate(count = ifelse(is.na(count), 0, count))

    }

    # Output varies according to args
    if (drop_NA) {
        data_events <- drop_empty_families(data_events)
    }
    if (drop_family) {
        data_events <- dplyr::select(data_events, -"strategy")
    }
    if (summary) {
      return(list(
        data_events = data_events,
        observed_events = with(data_events, unique(event[count > 0])),
        unobserved_events = with(data_events, unique(event[count == 0]))
      ))
    } else {
      return(data_events)
    }

}



#' Expand compact data object to data frame
#'
#' \code{expand_data} can be used to convert compact form data (one row per data type) to long form data (one row per observation).
#'
#' @rdname data_helpers
#' @inheritParams CausalQueries_internal_inherit_params
#' @return A \code{data.frame} with rows as data observation
#' @export
#' @examples
#' \donttest{
#' model <- make_model('X->M->Y')
#' make_events(model, n = 5) |>
#'   expand_data(model)
#' make_events(model, n = 0) |>
#'   expand_data(model)
#'  }
#'
expand_data <- function(data_events = NULL, model) {

  if (!is(model, "causal_model")) {
    stop("model should be a model generated with make_model")
  }

  if (is.null(data_events)) {
    data_events <- minimal_event_data(model)
  }

  if ((!is.data.frame(data_events) & !is.matrix(data_events)) |
      any(!c("event", "count") %in% colnames(data_events))) {
    stop(paste("data_events should be a data frame or a",
               "matrix with columns `event` and `count`"))
  }

  if ("strategy" %in% names(data_events)) {
    data_events <-
      dplyr::select(as.data.frame(data_events), c("event", "count"))
  }

  if (sum(data_events[, 2]) == 0) {
    return(minimal_data(model))  # Special case with no data
  }


  vars <- model$nodes
  df <- merge(get_all_data_types(model), data_events, by.x = "event")
  xx <- unlist(lapply(seq_len(nrow(df)), function(i) {
    replicate(df[i, ncol(df)], df[i, vars])
  }))
  out <- data.frame(matrix(xx, ncol = length(vars), byrow = TRUE))
  names(out) <- vars
  return(out)
}


#' Data type names
#'
#' Provides names to data types
#' @inheritParams CausalQueries_internal_inherit_params
#' @return A vector of strings of data types
#' @examples
#' model <- make_model('X -> Y')
#' data <- make_data(model, n = 2)
#' # CausalQueries:::data_type_names(model, data)
#' @keywords internal
#' @noRd

data_type_names <- function(model, data) {
  vars <- model$nodes
  data <- data[vars]
  data[data == ""] <- NA
  out <-
    apply(data, 1, function(j) {
      paste(paste0(vars[!is.na(j)], j[!is.na(j)]), collapse = "")
    })
  out[out == ""] <- "None"
  return(out)
}



#' Make data in long form
#'
#'\code{make_data} generates a dataset with one row per observation.
#'
#' @rdname data_helpers
#' @inheritParams CausalQueries_internal_inherit_params
#' @param param_type A character. String specifying type of parameters to
#'   make ("flat", "prior_mean", "posterior_mean", "prior_draw",
#'   "posterior_draw", "define"). With param_type set to \code{define} use
#'   arguments to be passed to \code{make_priors}; otherwise \code{flat} sets
#'   equal probabilities on each nodal type in each parameter set;
#'   \code{prior_mean}, \code{prior_draw}, \code{posterior_mean},
#'   \code{posterior_draw} take parameters as the means or as draws
#'   from the prior or posterior.
#' @param n Non negative integer. Number of observations.
#'   If not provided it is inferred from the  largest n_step.
#' @param n_steps A \code{list}. Number of observations to be
#'   observed at each step
#' @param given A string specifying known values on nodes, e.g. "X==1 & Y==1"
#' @param nodes A \code{list}. Which nodes to be observed at each step.
#'   If NULL all nodes are observed.
#' @param probs A \code{list}. Observation probabilities at each step
#' @param subsets A \code{list}. Strata within which observations are to be
#'   observed at each step. TRUE for all, otherwise an expression that
#'   evaluates to a logical condition.
#' @param complete_data A \code{data.frame}. Dataset with complete
#'   observations. Optional.
#' @param verbose Logical. If TRUE prints step schedule.
#' @param ... additional arguments that can be passed to
#'   \code{link{make_parameters}}
#' @return A \code{data.frame} with simulated data.
#' @export
#' @family data_generation
#'
#' @details
#' Note that default behavior is not to take account of whether a node has
#' already been observed when determining whether to select or not. One can
#' however specifically request observation of nodes that have not been
#' previously observed.
#' @examples
#'
#' # Simple draws
#' model <- make_model("X -> M -> Y")
#' make_data(model)
#' make_data(model, n = 3, nodes = c("X","Y"))
#' make_data(model, n = 3, param_type = "prior_draw")
#' make_data(model, n = 10, param_type = "define", parameters =  0:9)
#'
#' # Data Strategies
#' # A strategy in which X, Y are observed for sure and M is observed
#' # with 50% probability for X=1, Y=0 cases
#'
#' model <- make_model("X -> M -> Y")
#' make_data(
#'   model,
#'   n = 8,
#'   nodes = list(c("X", "Y"), "M"),
#'   probs = list(1, .5),
#'   subsets = list(TRUE, "X==1 & Y==0"))
#'
#'# n not provided but inferred from largest n_step (not from sum of n_steps)
#' make_data(
#'   model,
#'   nodes = list(c("X", "Y"), "M"),
#'   n_steps = list(5, 2))
#'
#' # Wide then deep
#'   make_data(
#'   model,
#'   n = 8,
#'   nodes = list(c("X", "Y"), "M"),
#'   subsets = list(TRUE, "!is.na(X) & !is.na(Y)"),
#'   n_steps = list(6, 2))
#'
# # Look for X only where X has not already been observed
#'
#' make_data(
#'   model,
#'   n = 8,
#'   nodes = list(c("X", "Y"), c("X", "M")),
#'   subsets = list(TRUE, "is.na(X)"),
#'   n_steps = list(3, 2))
#'
#'# Example with probabilities at each step
#'
#'make_data(
#'   model,
#'   n = 8,
#'   nodes = list(c("X", "Y"), c("X", "M")),
#'   subsets = list(TRUE, "is.na(X)"),
#'   probs = list(.5, .2))
#'
#'# Example with given data
#' make_data(model, given = "X==1 & Y==1", n = 5)

make_data <- function(
    model,
    n = 1,
    parameters = NULL,
    param_type = NULL,
    nodes    = NULL,
    n_steps = NULL,
    probs   = NULL,
    subsets = TRUE,
    complete_data = NULL,
    given = NULL,
    verbose = TRUE,
    ...){

  # n_step, n consistency
  if(!is.null(n)) {
    n_check(n)
  }

  if(!is.null(n_steps) & !is.null(n)) {
    if(max(n_steps |> unlist()) > n) {
      stop("n_step larger than n")
    }
  }

  if(!is.null(n_steps) & is.null(n)) {
    n <- max(n_steps |> unlist())
  }

  if(is.null(n_steps) & is.null(n)) {
    n <- 1
  }

  # n_steps and probs reconciliation
  if(!is.null(n_steps) & !is.null(probs)) {
    warning("Both `n_steps` and `prob` specified. `n_steps` overrides `probs`.")
  }

  if(is.null(n_steps) & is.null(probs)) {
    n_steps <- n
  }

  if(is.null(n_steps)) {
    n_steps <- NA
  }

  if(is.null(probs)) {
    probs <- 1
  }

  # Check that parameters sum to 1 in each param_set
  if(!is.null(parameters)) {
    parameters <- clean_param_vector(model, parameters)
  }

  # If parameters not provided, make or take from model
  if (is.null(parameters)) {
    if (!is.null(param_type)) {
      parameters <- make_parameters(model, param_type = param_type, ...)
    } else {
      parameters 	 <- get_parameters(model)
    }
  }

  # Check nodes
  if(is.null(nodes)) {
    nodes <- list(model$nodes)
  }

  if(!is.list(nodes)) {
    nodes <- list(nodes) # Lets one step nodes be provided as vector
  }

  if(!(all(unlist(nodes) %in% model$nodes))) {
    stop("All listed nodes should be in model")
  }

  # Complete data
  if(is.null(complete_data)) {
    complete_data <- make_data_single(model,
                                      n = n,
                                      parameters = parameters,
                                      given = given)
  }

  # Default behavior is to return complete data --
  # triggered if all data and all nodes sought in step 1
  if(all(model$nodes %in% nodes[[1]])) {
    if(probs[1]==1 || n_steps[1]==n) {
      return(complete_data)
    }
  }

  # Otherwise, gradually reveal
  # Check length consistency
  roster <-
    tibble(
      node_names = lapply(nodes, paste, collapse = ", ") |> unlist(),
      nodes = nodes,
      n_steps = n_steps |> unlist(),
      probs = probs |> unlist(),
      subsets = subsets |> unlist()
    )

  if (verbose) {
    print(roster)
  }

  observed <- complete_data

  observed[,] <- FALSE

  # Go step by step
  j <- 1
  while(j <= length(nodes)) {

    pars <- roster[j, ]

    observed <- observe_data(
      complete_data,
      observed = observed,
      nodes_to_observe = pars$nodes |> unlist(),
      prob = pars$probs,
      m    = pars$n_steps,
      subset = pars$subsets)

    j <- j + 1

  }

  observed_data <- complete_data
  observed_data[!observed] <- NA
  return(observed_data)
}



#' Observe data, given a strategy
#'
#' @param complete_data A \code{data.frame}. Data observed and unobserved.
#' @param observed A \code{data.frame}. Data observed.
#' @param nodes_to_observe  A list. Nodes to observe.
#' @param prob A scalar. Observation probability.
#' @param m A integer. Number of units to observe; if specified, \code{m}
#'   overrides \code{prob}.
#' @param subset A character.  Logical statement that can be applied to rows
#'   of complete data. For instance observation for some nodes might depend on
#'   observed values of other nodes; or observation may only be sought if
#'   data not already observed!
#' @return A \code{data.frame} with logical values indicating which nodes
#'   to observe in each row of `complete_data`.
#' @importFrom stats runif
#' @importFrom dplyr tibble
#' @keywords internal
#' @family data_generation
#' @examples
#' model <- make_model("X -> Y")
#' df <- make_data(model, n = 8)
#' # Observe X values only
#' CausalQueries:::observe_data(complete_data = df, nodes_to_observe = "X")
#' # Observe half the Y values for cases with observed X = 1
#' CausalQueries:::observe_data(complete_data = df,
#'      observed = CausalQueries:::observe_data(complete_data = df, nodes_to_observe = "X"),
#'      nodes_to_observe = "Y", prob = .5,
#'      subset = "X==1")

# A strategy consists of a. names of types to reveal  b. number of these
# to reveal c. subset from which to reveal them

observe_data <- function(complete_data,
                         observed = NULL,
                         nodes_to_observe = NULL,
                         prob = 1,
                         m = NULL,
                         subset = TRUE){

  if(is.null(observed)) {
    observed <- complete_data; observed[,] <- FALSE
  }

  if(is.null(nodes_to_observe)) {
    nodes_to_observe <- names(complete_data)
  }

  if(is.null(m)) {
    m <- NA
  }

  # Prep observed data data frame
  observed_data <- complete_data
  observed_data[!observed] <- NA

  # Within which subset to reveal?
  if(!is.logical(subset) & subset != "TRUE") {
    sub <- with(observed_data, eval(parse(text = subset)))
  } else {
    sub <- rep(TRUE, nrow(observed_data))
  }

  if(!any(sub)) {
    message("Empty subset")
  }

  # Target to reveal
  if(is.na(m)) {
    E <- prob*sum(sub) # Expected number selected
    m <- floor(E)  + (runif(1) <  E - floor(E)) # Get best m
  }

  observed[sample((seq_along(sub))[sub], m), nodes_to_observe] <- TRUE

  return(observed)
}


#' Generate full dataset
#'
#'
#' @inheritParams CausalQueries_internal_inherit_params
#' @param n An integer. Number of observations.
#' @param given A string specifying known values on nodes, e.g. "X==1 & Y==1"
#' @param parameters A numeric vector. Values of parameters may be specified.
#'   By default, parameters is drawn from priors.
#' @param param_type A character. String specifying type of parameters to make
#'   ("flat", "prior_mean", "posterior_mean", "prior_draw",
#'   "posterior_draw", "define). With param_type set to \code{define} use
#'   arguments to be passed to \code{make_priors}; otherwise \code{flat} sets
#'   equal probabilities on each nodal type in each parameter set;
#'   \code{prior_mean}, \code{prior_draw}, \code{posterior_mean},
#'   \code{posterior_draw} take parameters as the means or as draws
#'   from the prior or posterior.
#' @param w Vector of event probabilities can be provided directly.
#'   This is useful for speed for repeated data draws.
#' @param P A \code{matrix}. Parameter matrix that can be used to
#'   generate w if w is not provided
#' @param A A \code{matrix}. Ambiguity matrix that can be used
#'   to generate w if w is not provided
#' @return A \code{data.frame} of simulated data.
#' @keywords internal
#' @family data_generation
#'
#' @examples
#'
#' model <- make_model("X -> Y")
#'
#' # Simplest behavior uses by default the parameter vector contained in model
#' CausalQueries:::make_data_single(model, n = 5)
#'
#' CausalQueries:::make_data_single(model, n = 5, param_type = "prior_draw")
#'
#' # Simulate multiple datasets. This is fastest if
#' # event probabilities (w) are  provided
#' w <- get_event_probabilities(model)
#' replicate(5, CausalQueries:::make_data_single(model, n = 5, w = w))
#'

make_data_single <- function(
    model,
    n = 1,
    parameters = NULL,
    param_type = NULL,
    given = NULL,
    w = NULL,
    P = NULL,
    A = NULL) {

  # Check that parameters sum to 1 in each param_set
  # if(!is.null(parameters)) parameters <- clean_param_vector(model, parameters)

  # If parameters not provided, take from model
  if(is.null(parameters)) {
    if(!is.null(param_type)) {
      parameters <- make_parameters(model, param_type = param_type)
    } else {
      parameters 	 <- get_parameters(model)
    }
  }

  # Generate event probabilities w if missing
  if(is.null(w)) {
    w <- get_event_probabilities(
      model,
      parameters = parameters,
      A = A,
      P = P,
      given = given)
  }

  # Data drawn here
  make_events(model, n = n,  parameters = parameters, w = w) |>
    expand_data(model)

}



#' Make data in compact form
#'
#' \code{make_events} generates a dataset with one row for each data type.
#' Draws full data only. To generate various types of incomplete data see
#' \code{\link{make_data}}.
#'
#'
#' @rdname data_helpers
#' @inheritParams CausalQueries_internal_inherit_params
#' @param n An integer. Number of observations.
#' @param w A numeric matrix. A `n_parameters x 1` matrix of event
#'   probabilities with named rows.
#' @param param_type A character. String specifying type of parameters to make
#'   'flat', 'prior_mean', 'posterior_mean', 'prior_draw', 'posterior_draw',
#'   'define. With param_type set to \code{define} use arguments to be passed
#'   to \code{make_priors}; otherwise \code{flat} sets equal probabilities on
#'   each nodal type in each parameter set; \code{prior_mean},
#'   \code{prior_draw}, \code{posterior_mean}, \code{posterior_draw} take
#'   parameters as the means or as draws from the prior or posterior.
#' @param include_strategy Logical. Whether to include a 'strategy' vector.
#'   Defaults to FALSE. Strategy vector does not vary with full data but
#'   expected by some functions.
#' @param ... Arguments to be passed to make_priors if
#'   param_type == \code{define}
#' @return A \code{data.frame} of events
#' @importFrom stats rmultinom
#' @export
#' @family data_generation
#'
#' @examples
#' \donttest{
#' model <- make_model('X -> Y')
#' make_events(model = model)
#' make_events(model = model, param_type = 'prior_draw')
#' make_events(model = model, include_strategy = TRUE)
#' }
#'
make_events <- function(model,
                        n = 1,
                        w = NULL,
                        P = NULL,
                        A = NULL,
                        parameters = NULL,
                        param_type = NULL,
                        include_strategy = FALSE, ...) {
  # Check whether w is a matrix with named rows
  if (!is.null(w)) {
    if (!is.matrix(w)) {
      stop("w has to be a matrix.")
    }
    if (length(rownames(w)) != nrow(w)) {
      stop("w has to be a matrix with named rows.")
    }
  }

  # Check that parameters sum to 1 in each param_set
  if (!is.null(parameters)) {
    parameters <- clean_param_vector(model, parameters)
  }

  # If parameters not provided, take from model
  if (is.null(parameters)) {
    if (!is.null(param_type)) {
      parameters <- make_parameters(model, param_type = param_type, ...)
    } else {
      parameters <- get_parameters(model)
    }
  }

  if (is.null(w)) {
    if (is.null(P)) {
      P <- get_parameter_matrix(model)
    }
    if (is.null(A)) {
      A <- get_ambiguities_matrix(model)
    }
    w <- get_event_probabilities(model = model,
                                 P = P,
                                 A = A,
                                 parameters = parameters)
  }

  # Draw events (Compact data frame)
  df <- data.frame(event = rownames(w), count = rmultinom(1, n, w))

  if (include_strategy) {
    df$strategy <- paste0(model$nodes, collapse = "")
    df <- df[, c("event", "strategy", "count")]
  }

  return(df)
}


#' Creates a compact data frame for case with no data
#' @param model A \code{causal_model}. A model object generated by
#'   \code{\link{make_model}}.
#' @return A compact data frame where each row represents an element from the
#'   exhaustive set of events of a model. The count for each event is
#'   set to zero.
#' @keywords internal
#' @noRd
#' @examples
#' \donttest{
#' model <- make_model('X -> K -> Y')
#' CausalQueries:::minimal_event_data(model)
#' }

minimal_event_data <- function(model){
  make_data(model, n = 1) |>
    collapse_data(model) |>
    mutate(count = 0)
}


#' Creates a data frame for case with no data
#' @param model A \code{causal_model}. A model object generated by
#'   \code{\link{make_model}}.
#' @return A \code{data.frame} with one row of NAs and columns named according
#'   to nodes in a model.
#' @keywords internal
#' @noRd
#' @examples
#' \donttest{
#' model <- make_model('X -> K -> Y')
#' CausalQueries:::minimal_data(model)
#' }
minimal_data <- function(model) {
  vars <- model$nodes
  df <- data.frame(t(rep(NA, length(vars))))
  names(df) <- vars
  return(df)
}



#' simulate_data is a deprecated alias for make_data
#'
#' @param ... arguments for \code{\link{make_model}}
#' @return A \code{data.frame} with simulated data.
#' @keywords internal
#' @family data_generation
#' @noRd
simulate_data <- function(...) {
  .Deprecated("make_data")
  make_data(...)
}



#' Get all data types
#'
#' Creates data frame with all data types (including NA types)
#' that are possible from a model.
#'
#' @inheritParams CausalQueries_internal_inherit_params
#' @param complete_data Logical. If `TRUE` returns only complete data types
#'   (no NAs). Defaults to `FALSE`.
#' @param possible_data Logical. If `TRUE` returns only complete data types
#'   (no NAs) that are *possible* given model restrictions. Note that in
#'   principle an intervention could make observationally impossible data types
#'   arise. Defaults to `FALSE`.
#' @param given A character.  A quoted statement that evaluates to logical.
#'   Data conditional on specific values.
#' @return A \code{data.frame} with all data types (including NA types)
#'   that are possible from a model.
#' @export
#' @family data_generation
#' @examples
#' \donttest{
#' make_model('X -> Y') |> get_all_data_types()
#' model <- make_model('X -> Y') |>
#'   set_restrictions(labels = list(Y = '00'), keep = TRUE)
#'   get_all_data_types(model)
#'   get_all_data_types(model, complete_data = TRUE)
#'   get_all_data_types(model, possible_data = TRUE)
#'   get_all_data_types(model, given  = 'X==1')
#'   get_all_data_types(model, given  = 'X==1 & Y==1')
#'}
get_all_data_types <- function(model,
                               complete_data = FALSE,
                               possible_data = FALSE,
                               given = NULL) {
  nodes <- model$nodes
  # If complete_data allow only 2 possible
  # realizations (0,1), otherwise 3 (0,1,NA)
  r <- ifelse(complete_data, 1, 2)
  m <- length(model$nodes)
  df <- data.frame(perm(rep(r, m))) - 1 + complete_data
  df[df == -1] <- NA
  names(df) <- nodes
  df <- data.frame(cbind(event = data_type_names(model, df), df))

  order_list <-
    c(list(rowSums(is.na(df))),
      lapply(rev(nodes), function(node)
        is.na(df[, node])),
      lapply(rev(nodes), function(node)
        df[, node]))

  df <- df[do.call(what = order, args = order_list), ]

  if (possible_data) {
    possible_data_types <-
      unique(data_type_names(model, realise_outcomes(model)))
    df <- dplyr::filter(df, event %in% possible_data_types)
  }

  # exclude data not consistent with 'given'
  # (NAs are *not* consistent with given)
  if (!is.null(given)) {
    take <- with(df, eval(parse(text = given)))
    take[is.na(take)] <- FALSE
    df <- df[take, ]
  }
  rownames(df) <- df$event

  return(df)
}

#' get_data_families
#'
#' Get possible data types
#'
#' @inheritParams CausalQueries_internal_inherit_params
#' @param drop_impossible Logical. Whether to drop data that is impossible given
#'   model restrictions. Defaults to `TRUE`.
#' @param drop_all_NA Logical. Whether to drop row of all `NA`s.
#'   Defaults to `TRUE`
#' @param mapping_only Logical. Whether to return data mapping matrix only.
#'   Defaults to `FALSE`.
#' @return Returns indices and ambiguity matrix
#' @keywords internal
#' @noRd
#'
#' @importFrom dplyr filter
#' @examples
#' \donttest{
#' CausalQueries:::get_data_families(model = make_model('X->Y'))
#' CausalQueries:::get_data_families(model = make_model('X->Y'),
#'                                   mapping_only = TRUE)
#' CausalQueries:::get_data_families(model = make_model('X-> M -> Y'))
#'
#' }

get_data_families <- function(model,
                              drop_impossible = TRUE,
                              drop_all_NA = TRUE,
                              mapping_only = FALSE) {

  event <- NULL

  # Get nodes
  nodes <- model$nodes

  # Get all possible data realizations, given strategies in
  # which some data is not sought (NA).
  all_data <- get_all_data_types(model)

  # Get the realizations of the fundamental *possible* data events
  possible_data_types <-
    unique(data_type_names(model, realise_outcomes(model)))
  full_data <-
    filter(all_data, apply(all_data[, -1, drop = FALSE], 1,
                           function(j)
                             ! any(is.na(j)))) |>
    filter(event %in% possible_data_types)

  # Make E: Sign matrix used to see if data is
  # *inconsistent* with reduced type
  sign_matrix <- (2 * as.matrix(all_data[nodes]) - 1)
  sign_matrix[is.na(sign_matrix)] <- 0

  type_matrix <- (2 * (as.matrix(full_data[nodes])) - 1)


  E <- 1 * matrix(
    apply(sign_matrix, 1,
          function(j)
            apply(type_matrix, 1,
                  function(k)
                    ! (any(
                      k * j == -1
                    )))),
    nrow = length(all_data$event),
    byrow = TRUE
  )

  rownames(E) <- all_data$event
  colnames(E) <- full_data$event

  # Filtering
  keep <- rep(TRUE, nrow(E))
  if (drop_impossible)
    keep[!(apply(E, 1, function(j) any(j == 1)))] <- FALSE
  if (drop_all_NA)
    keep[rownames(E) == "None"] <- FALSE

  E <- E[keep,, drop = FALSE]


  all_data <- all_data[keep,]

  possible_events <- rownames(E)

  ## STRATEGIES ##############################

  # Figure out what strategy is being used in each of
  # the possible data realizations
  which_strategy <-
    apply(all_data[nodes], 1, function(row)
      nodes[!is.na(row)])
  which_strategy <-
    which_strategy[lapply(which_strategy, length) != 0]

  if (!mapping_only) {
    E <-
      data.frame(
        event = possible_events,
        strategy = unlist(lapply(which_strategy, paste, collapse = "")),
        E,
        stringsAsFactors = FALSE
      )

    rownames(E) <- E$event
  }

  return(E)


}

#' Drop empty families
#'
#' @inheritParams CausalQueries_internal_inherit_params
#'
#' @return Returns data events with strategies (excluding  strategy families
#'   that contain no observed data)
#' @keywords internal
#' @noRd
#' @examples
#'\donttest{
#' data_events <- data.frame(event = c('X0Y0', 'Y0'),
#'                           strategy = c('XY', 'Y'),
#'                           count = 1:0)
#' CausalQueries:::drop_empty_families(data_events)
#' }
#'
drop_empty_families <- function(data_events) {

  for (j in unique(data_events$strategy)) {
    if (sum(data_events$count[data_events$strategy == j]) == 0) {
      data_events <- dplyr::filter(data_events, strategy != j)
    }
  }
  return(data_events)

}



