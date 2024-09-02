
#' Print a short summary for a causal model
#'
#' print method for class \code{causal_model}.
#'
#' @param x An object of \code{causal_model} class, usually a result of
#'   a call to \code{make_model} or \code{update_model}.
#' @param ... Further arguments passed to or from other methods.
#'
#' @details
#' The information regarding the causal model includes the statement describing
#' causal relations using \link{dagitty} syntax,
#' number of nodal types per parent in a DAG, and number of causal types.
#'
#' @export
print.causal_model <- function(x, ...) {
  print.statement(x$statement)

  cat("\nNumber of types by node:\n")
  nodal_types <- get_nodal_types(x)
  print(vapply(nodal_types , length, numeric(1) ,USE.NAMES = TRUE))


  if (!is.null(x$causal_types)) {
    cat("\nNumber of unit types:")
    cat(paste0(" ", nrow(get_causal_types(x)), "\n\n"))
  }

  if (!is.null(x$posterior_distribution)) {
    cat("\nModel has been updated and contains a posterior distribution with\n")
    cat(paste(grab(x, object = "stan_summary")[[2]],"\n"))
    cat("Use grab(model, object = 'stan_summary') to inspect stan summary \n\n")
  }

  return(invisible(x))
}

#' Summarizing causal models
#'
#' summary method for class \code{causal_model}.
#'
#' @param object An object of \code{causal_model} class produced using
#'   \code{make_model} or \code{update_model}.
#' @param ... Further arguments passed to or from other methods.
#'
#' @details
#' \code{print.summary.causal_model} reports DAG data frame, full specification of
#' nodal types and summary of model restrictions in addition to standard
#' \code{print.causal_model} output.
#'
#' @export
summary.causal_model <- function(object, ...) {
  structure(object, class = c("summary.causal_model", "data.frame"))
}

#' @rdname summary.causal_model
#'
#' @param x An object of \code{summary.causal_model} class, usually a result of
#'   a call to \code{summary.causal_model}.
#' @param stanfit Logical. Whether to include readable summary of
#'   \code{stanfit} produced when updating a model via \code{update_model}.
#'   Defaults to `FALSE`.
#' @param ... Further arguments passed to or from other methods.
#'
#' @export
print.summary.causal_model <- function(x, stanfit = FALSE, ... ) {

  if (stanfit & is.null(x$stan_objects))
    warning(paste0("You requested summary of stan fit on the causal_model",
                   "that was not updated. The summary of stan fit will",
                   "not be printed"))

  print.statement(x$statement)
  print.dag(x$dag)
  cat("\n------------------------------------------------------------------\n\n")
  print.nodal_types(x$nodal_types)

  if (!is.null(x$causal_types)) {
    cat("\nNumber of unit types:")
    cat(paste0("  ", nrow(x$causal_types), "\n\n"))
  }

  if (!is.null(attr(x, "restrictions"))) {
    restrictions <- attr(x, "restrictions")
    cat("----------------------------------------------------------------\n")
    cat("\nRestrictions: \n")
    for (node in x$nodes) {
      cat(paste0(node,
                 ": ",
                 length(restrictions[[node]]),
                 " restricted types \n\n"))
    }
  }

  if (stanfit & !is.null(x$stan_objects)) {
    cat("----------------------------------------------------------------\n\n")
    print.stan_summary(x$stan_objects$stan_summary)
    cat("\n")
  }

  return(invisible(x))
}

#' Print a short summary for a causal_model statement
#'
#' print method for class \code{statement}.
#'
#' @param x An object of \code{statement} class, which is a sub-object of
#'    an object of the \code{causal_model} class produced using
#'    \code{make_model} or \code{update_model}.
#' @param ... Further arguments passed to or from other methods.
#'
#' @export
print.statement <- function(x, ...) {
  cat("\nStatement: \n")
  cat(x)
  cat("\n")
  return(invisible(x))
}

#' Print a short summary for causal_model nodes
#'
#' print method for class \code{nodes}.
#'
#' @param x An object of \code{nodes} class, which is a sub-object of
#'    an object of the \code{causal_model} class produced using
#'    \code{make_model} or \code{update_model}.
#' @param ... Further arguments passed to or from other methods.
#'
#' @export
print.nodes <- function(x, ...) {
  cat("\nNodes: \n")
  cat(paste(x, collapse = ", "))
  return(invisible(x))
}

#' Print a short summary for a causal_model parents data-frame
#'
#' print method for class \code{parents_df}.
#'
#' @param x An object of \code{parents_df} class, which is a sub-object of
#'    an object of the \code{causal_model} class produced using
#'    \code{make_model} or \code{update_model}.
#' @param ... Further arguments passed to or from other methods.
#'
#' @export
print.parents_df <- function(x, ...) {
  cat("\nRoot vs Non-Root status with number and names of parents for each node: \n")
  base::print.data.frame(x)
  return(invisible(x))
}

#' Print a short summary for a causal_model parameters data-frame
#'
#' print method for class \code{parameters_df}.
#'
#' @param x An object of \code{parameters_df} class, which is a sub-object of
#'    an object of the \code{causal_model} class produced using
#'    \code{make_model} or \code{update_model}.
#' @param ... Further arguments passed to or from other methods.
#'
#' @export
print.parameters_df <- function(x, ...) {
  cat("Mapping of model parameters to nodal types: \n\n")
  cat("----------------------------------------------------------------\n")
  cat("\n param_names: name of parameter")
  cat("\n node: name of endogeneous node associated with the parameter")
  cat("\n gen: partial causal ordering of the parameter's node")
  cat("\n param_set: parameter groupings forming a simplex")
  cat("\n given: if model has confounding gives conditioning nodal type")
  cat("\n param_value: parameter values")
  cat("\n priors: hyperparameters of the prior Dirichlet distribution \n\n")
  cat("----------------------------------------------------------------\n\n")
  if(nrow(x) > 10) {
    cat("\n first 10 rows: \n")
    print.data.frame(x[1:10,])
  } else {
    print.data.frame(x)
  }
  return(invisible(x))
}

#' Print a short summary for causal_model causal-types
#'
#' print method for class \code{causal_types}.
#'
#' @param x An object of \code{causal_types} class, which is a sub-object of
#'    an object of the \code{causal_model} class produced using
#'    \code{make_model} or \code{update_model}.
#' @param ... Further arguments passed to or from other methods.
#'
#' @export
print.causal_types <- function(x, ...) {
  cat("\nCausal Types: ")
  cat("\ncartesian product of nodal types\n\n")
  if(nrow(x) > 10) {
    cat("\n first 10 causal types: \n")
    print.data.frame(x[1:10,])
  } else {
    print.data.frame(x)
  }
  return(invisible(x))
}

#' Print a short summary for causal_model nodal-types
#'
#' print method for class \code{nodal_types}.
#'
#' @param x An object of \code{nodal_types} class, which is a sub-object of
#'    an object of the \code{causal_model} class produced using
#'    \code{make_model} or \code{update_model}.
#' @param ... Further arguments passed to or from other methods.
#'
#' @export
print.nodal_types <- function(x, ...) {
  cat("Nodal types: \n")

  nodal_types <- x
  nodes <- names(x)

  for (n in nodes) {
    nt <- nodal_types[[n]]
    interpret <- attr(nodal_types, "interpret")[[n]]
    stop_at <- min(length(nt), 16)
    cat(paste0("$", n, "\n"))

    cat(paste0(nt[1:stop_at], collapse = "  "))

    if (stop_at != length(nt)) {
      cat(paste0(" ...", length(nt) - 16, " nodal types omitted"))
    }
    cat("\n\n")
    print(interpret)
    cat("\n")
  }

  cat("\nNumber of types by node\n")

  print(vapply(nodal_types , length, numeric(1), USE.NAMES = TRUE))

  return(invisible(x))
}

#' Print a short summary for causal_model parameters
#'
#' print method for class \code{parameters}.
#'
#' @param x An object of \code{parameters} class, which is a sub-object of
#'    an object of the \code{causal_model} class produced using
#'    \code{make_model} or \code{update_model}.
#' @param ... Further arguments passed to or from other methods.
#'
#' @export
print.parameters <- function(x, ...) {
  cat("Model parameters with associated probabilities: \n\n")
  cat(names(x))
  cat("\n")
  cat(x)
  return(invisible(x))
}

#' Print a short summary for causal_model parameter prior distributions
#'
#' print method for class \code{parameters_prior}.
#'
#' @param x An object of \code{parameters_prior} class, which is a sub-object of
#'    an object of the \code{causal_model} class produced using
#'    \code{set_prior_distribution}.
#' @param ... Further arguments passed to or from other methods.
#'
#' @export
print.parameters_prior <- function(x, ...) {
  cat("Summary statistics of model parameter prior distributions:")
  cat(paste("\nDimensions:", dim(x)[1], "rows (draws) by", dim(x)[2], "cols (parameters) \n\n", sep = " "))
  cat("Summary: \n\n")
  distribution_summary <- as.data.frame(t(apply(x, 2, summarise_distribution)))
  rounding_threshold <- find_rounding_threshold(distribution_summary)
  print.data.frame(round(distribution_summary, rounding_threshold))
  return(invisible(x))
}

#' Print a short summary for causal_model parameter posterior distributions
#'
#' print method for class \code{parameters_posterior}.
#'
#' @param x An object of \code{parameters_posterior} class, which is a sub-object of
#'    an object of the \code{causal_model} class produced using
#'    \code{update_model}.
#' @param ... Further arguments passed to or from other methods.
#'
#' @export
print.parameters_posterior <- function(x, ...) {
  cat("Summary statistics of model parameter posterior distributions:")
  cat(paste("\n:", dim(x)[1], "rows (draws) by", dim(x)[2], "cols (parameters)\n\n", sep = " "))
  distribution_summary <- as.data.frame(t(apply(x, 2, summarise_distribution)))
  rounding_threshold <- find_rounding_threshold(distribution_summary)
  print.data.frame(round(distribution_summary, rounding_threshold))
  return(invisible(x))
}


#' Print a short summary for causal-type prior distributions
#'
#' print method for class \code{type_prior}.
#'
#' @param x An object of \code{type_prior} class, which is a sub-object of
#'    an object of the \code{causal_model} class produced using
#'    \code{make_model} or \code{update_model}.
#' @param ... Further arguments passed to or from other methods.
#'
#' @export
print.type_prior <- function(x, ...) {
  cat("Summary statistics of causal type prior distributions:")
  cat(paste("\nDimensions:", dim(x)[1], "rows (draws) by", dim(x)[2], "cols (types) \n\n", sep = " "))
  cat("Summary: \n\n")
  distribution_summary <- as.data.frame(t(apply(x, 2, summarise_distribution)))
  rounding_threshold <- find_rounding_threshold(distribution_summary)
  print.data.frame(round(distribution_summary, rounding_threshold))
  return(invisible(x))
}

#' Print a short summary for paramater mapping matrix
#'
#' print method for class \code{parameter_mapping}.
#'
#' @param x An object of \code{parameter_mapping} class.
#' @param ... Further arguments passed to or from other methods.
#'
#' @export
print.parameter_mapping <- function(x,  ...) {
  cat("\nParameter mapping matrix: \n\n")
  cat("Maps from parameters to data types, with \n")
  cat("possibly multiple columns for each data type \n")
  cat("in cases with confounding. \n\n")
  print(data.frame(x))
  cat("\n")
  return(invisible(x))
}


#' Print a short summary for stan fit
#'
#' print method for class \code{stan_summary}.
#'
#' @param x An object of \code{stan_summary} class, which is a sub-object of
#'    an object of the \code{causal_model} class produced using
#'    \code{update_model}.
#' @param ... Further arguments passed to or from other methods.
#'
#' @export
print.stan_summary <- function(x, ...) {
  cat(x, sep = "\n")
  return(invisible(x))
}


#' helper to compute mean and sd of a distribution data.frame
#' @param x An object for summarizing
summarise_distribution <- function(x) {
  summary <- c(mean(x, na.rm = TRUE), sd(x, na.rm = TRUE))
  names(summary) <- c("mean", "sd")
  return(summary)
}

#' helper to find rounding thresholds for print methods
#' @param x An object for rounding
find_rounding_threshold <- function(x) {
  x <- max(abs(x)) - min(abs(x))
  pow <- 1
  x_pow <- x * 10^pow

  while(x_pow < 1) {
    pow <- pow + 1
    x_pow <- x * 10^pow
  }

  return(pow + 1)
}



#' Print a short summary of posterior_event_probabilities
#'
#' print method for class \code{posterior_event_probabilities}.
#'
#' @param x An object of \code{posterior_event_probabilities} class.
#' @param ... Further arguments passed to or from other methods.
#'
#' @export
#'
print.posterior_event_probabilities <-
  function(x, ...) {
    cat("\nPosterior draws of event probabilities (transformed parameters)\n")
    cat(paste("\nDimensions:", dim(x)[1], "rows (draws) by", dim(x)[2], "cols (data types)\n\n", sep = " "))
    cat("Summary: \n\n")
    distribution_summary <- as.data.frame(t(apply(x, 2, summarise_distribution)))
    rounding_threshold <- find_rounding_threshold(distribution_summary)
    print.data.frame(round(distribution_summary, rounding_threshold))
    return(invisible(x))
  }

#' Print a short summary for event probabilities
#'
#' print method for class \code{event_probabilities}.
#'
#' @param x An object of \code{event_probabilities} class, which is a sub-object of
#'    an object of the \code{causal_model} class produced using
#'    \code{update_model}.
#' @param ... Further arguments passed to or from other methods.
#'
#' @export
print.event_probabilities <- function(x, ...) {
  cat("\nThe probability of observing a given combination of data ")
  cat("\nrealizations for a given set of parameter values.\n\n")
  print.data.frame(data.frame(event_probs = x))
  return(invisible(x))
}


#' Print a short summary for causal-type posterior distributions
#'
#' print method for class \code{type_distribution}.
#'
#' @param x An object of \code{type_distribution} class, which is a sub-object of
#'    an object of the \code{causal_model} class produced using
#'    \code{get_type_prob_multiple}.
#' @param ... Further arguments passed to or from other methods.
#'
#' @export
print.type_distribution <- function(x, ...) {
  cat("Posterior draws of causal types (transformed parameters)")
  cat(paste("\nDimensions:", dim(x)[1], "rows (draws) by", dim(x)[2], "cols (types) \n\n", sep = " "))
  cat("Summary: \n\n")
  distribution_summary <- as.data.frame(t(apply(x, 2, summarise_distribution)))
  rounding_threshold <- find_rounding_threshold(distribution_summary)
  print.data.frame(round(distribution_summary, rounding_threshold))
  return(invisible(x))
}



#' Print a tightened summary of model queries
#'
#' print method for class \code{model_query}.
#'
#' @param x An object of \code{model_query} class.
#' @param ... Further arguments passed to or from other methods.
#'
#' @export
#'
print.model_query <- function(x, ...) {

  cred.low <- NULL
  cred.high <- NULL
  case_level <- NULL
  given <- NULL

  text1 <- "Causal queries generated by query_model"

  # Output simplification
  if (all(c("using", "case_level", "sd", "given") %in% names(x))) {
    if (all(x$using == "parameters" | x$case_level)) {
      x <- x |>
        dplyr::select(-sd,-cred.low,-cred.high)
    }
    if (all(x$given == "-")) {
      x <- x |>
        dplyr::select(-given)
    }
    if (all(!x$case_level)) {
      x <- x |>
        dplyr::select(-case_level)
      text1 <- paste(text1, "(all at population level)")
    }
  }

  cat("\n")
  cat(text1)
  print(knitr::kable(x, digits = 3))
  cat("\n")
  return(invisible(x))
}


#' Print a short summary for list of parents
#'
#' print method for class \code{parents}.
#'
#' @param x An object of the \code{parents} class, which is a list
#' containing the names of each parent node in the \code{causal_model} produced using
#' \code{get_parents}.
#' @param ... Further arguments passed to or from other methods.
#'
#' @export
print.parents <- function(x, ...) {
  cat("Parents: \n\n")
  nodes <- names(x)

  for (n in nodes) {
    n_parents <- x[[n]]
    cat(paste0("$", n, "\n"))
    if(length(n_parents)==0)
      cat("Node has no parents \n")
    else
      cat(paste(n_parents, sep = " "))
    cat("\n" )
  }
  return(invisible(x))
}



#' Print a short summary for stan_objects
#'
#' print method for class \code{stan_objects}.
#'
#' @param x An object of the \code{stan_objects} class, which is a list
#' containing the data, \code{type_distribution} and  \code{stan_summary}.
#' @param ... Further arguments passed to or from other methods.
#'
#' @export
print.stan_objects <- function(x, ...) {
  cat("stan_objects: \n")
  base::print.listof(x)
  return(invisible(x))
}

