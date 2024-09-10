
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

  cat("\nStatement: \n")
  cat(x$statement)
  cat("\n")

  cat("\nNumber of types by node:\n")
  print(vapply(get_nodal_types(x) , length, numeric(1) ,USE.NAMES = TRUE))


  if (!is.null(x$causal_types)) {
    cat("\nNumber of unit types:")
    cat(paste0(" ", nrow(get_causal_types(x)), "\n\n"))
  }

  if (!is.null(x$posterior_distribution) & !is.null(x$stan_objects)) {
    cat("\nModel has been updated and contains a posterior distribution with\n")
    cat(paste(x$stan_objects$stan_summary[[2]],"\n"))
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
#' @return Returns the object of class \code{summary.causal_model} that preserves the list structure of \code{causal_model} class and adds the following additional elements:
#' \itemize{
#'   \item \code{"causal_types"} a data frame listing causal types and the nodal types that produce them,
#'   \item \code{"data_types"} a list with the all data  types consistent with the model; for options see \code{"?get_all_data_types"},
#'   \item \code{"event_probabilities"}  a vector of data (event) probabilities given a parameter vector; for options see \code{"?get_event_probabilities"},
#'   \item \code{"ambiguities_matrix"} a matrix mapping from causal types into data types,
#'   \item \code{"parameters"} a vector of 'true' parameters,
#'   \item \code{"parameter_names"} a vector of names of parameters,
#'   \item \code{"parameter_mapping"} a matrix mapping from parameters into data types,
#'   \item \code{"parameter_matrix"} a matrix mapping from parameters into causal types,
#'   \item \code{"prior_hyperparameters"} a vector of alpha values used to parameterize Dirichlet prior distributions; optionally provide node names to reduce output \code{"grab(prior_hyperparameters, c('M', 'Y'))"}
#' }
#'
#' @export
summary.causal_model <- function(object, ...) {

  if (class(object) != "causal_model")
    stop("This summary function works with causal_model objects only")

  object$causal_types <- get_causal_types(object)

  # object$nodal_types <- get_nodal_types(object)

  object$data_types <- get_all_data_types(object)

  object$event_probabilities <- get_event_probabilities(object)

  object$ambiguities_matrix <- get_ambiguities_matrix(object)

  object$parameters <- get_parameters(object)

  object$parameter_names <- get_parameter_names(object)

  object$parameter_mapping <- get_parmap(object)

  object$parameter_matrix <- get_parameter_matrix(object)

  object$prior_hyperparameters <- get_priors(object)

  object$prior_distribution <- get_param_dist(object, using = "priors")

  object$type_prior <- get_type_prob_multiple(object, using = "priors")

  object <- structure(object, class = c("summary.causal_model"))

  return(object)

}

#' @rdname summary.causal_model
#'
#' @param x An object of \code{summary.causal_model} class, produced using \code{summary.causal_model}.
#' @param include A character string specifying the objects summaries to print in addition to causal statement. Defaults to \code{NULL} printing DAG data frame, specification of nodal types and summary of model restrictions. See \link[=details]{Details} for full list of available values.
#' @param ... Further arguments passed to or from other methods.
#'
#'
#' @details
#'
#' \code{print.summary.causal_model} reports causal statement, DAG data frame, full specification of nodal types and summary of model restrictions. By specifying \code{include} argument users can instead print a custom summary of any set of the following objects contained in the \code{summary.causal_model}:
#' \itemize{
#'   \item \code{"dag"} A data frame with columns ‘parent’ and ‘children’ indicating how nodes relate to each other,
#'   \item \code{"nodes"} A list containing the nodes in the model,
#'   \item \code{"parents_df"} a data frame listing nodes, whether they are root nodes or not, and the number and names of parents they have,
#'   \item \code{"parameters"} a vector of 'true' parameters,
#'   \item \code{"parameter_names"} a vector of names of parameters,
#'   \item \code{"parameter_mapping"} a matrix mapping from parameters into data types,
#'   \item \code{"parameter_matrix"} a matrix mapping from parameters into causal types,
#'   \item \code{"parameters_df"} a data frame containing parameter information,
#'   \item \code{"causal_types"} a data frame listing causal types and the nodal types that produce them,
#'   \item \code{"causal_types_interpretation"} a key to interpreting types; see \code{"?interpret_type"} for options,
#'   \item \code{"nodal_types"} a list with the nodal types of the model,
#'   \item \code{"data_types"} a list with the all data  types consistent with the model; for options see \code{"?get_all_data_types"},
#'   \item \code{"event_probabilities"}  a vector of data (event) probabilities given a parameter vector; for options see \code{"?get_event_probabilities"},
#'   \item \code{"ambiguities_matrix"} a matrix mapping from causal types into data types,
#'   \item \code{"prior_hyperparameters"} a vector of alpha values used to parameterize Dirichlet prior distributions; optionally provide node names to reduce output \code{"grab(prior_hyperparameters, c('M', 'Y'))"}
#'   \item \code{"prior_distribution"} a data frame of the parameter prior distribution,
#'   \item \code{"posterior_distribution"} a data frame of the parameter posterior distribution,
#'   \item \code{"posterior_event_probabilities"} a sample of data (event) probabilities from the posterior,
#'   \item \code{"type_distribution"} a matrix of type probabilities using posteriors,
#'   \item \code{"type_prior"} a matrix of type probabilities using priors,
#'   \item \code{"stan_objects"} stan_objects is a list of Stan outputs that can include the stanfit object, the data that was used, and distributions over causal types and event probabilities.
#' }
#'
#'
#' @export
print.summary.causal_model <- function(x, include = NULL, ... ) {

  # create message for updating
  printout <- "Model does not contain"
  printout2 <- "To include it update model"

  if (is.null(x$posterior_distribution) | is.null(x$stan_objects)) {

    printout <-
      c(printout, "posterior_distribution or stan_objects")

  } else {
    if (is.null(x$stan_objects$data)) {

      printout <- c(printout, "data")
      printout2 <- c(printout2, "data")
    }

    if (is.null(x$stan_objects$event_probabilities)) {

      printout <-
        c(printout, "event_probabilities")
      printout2 <-
        c(printout2, "keep_event_probabilities = TRUE")

    }

    if (is.null(x$stan_objects$type_distribution)) {

      printout <-
        c(printout, "type_distribution")
      printout2 <-
        c(printout2, "type_distribution = TRUE")

    }
  }

  if (length(printout) > 1) {
    if (length(printout2) > 1) {
      message(
        paste0(printout[1], " ", paste0(printout[-1], collapse = ", "), ". ",
               printout2[1], " with ", paste0(printout2[-1], collapse = ", "), ".")
      )
    } else {
      message(
        paste0(printout[1], " ", paste0(printout[-1], collapse = ", "), ". ",
               printout2[1], ".")
      )
    }
  }

  cat("\nStatement: \n")
  cat(x$statement)
  cat("\n\n------------------------------------------------------------------\n")


  if (is.null(include)) {

    cat("\nDag:\n\n")
    print(x$dag)
    cat("\n")

    cat("\nNodal types: \n")

    nodal_types <- x$nodal_types
    nodes <- names(x$nodal_types)

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

    cat("\nNumber of types by node:\n")

    print(vapply(nodal_types , length, numeric(1), USE.NAMES = TRUE))

    cat("\n------------------------------------------------------------------\n")

    if (!is.null(x$causal_types)) {
      cat("\nNumber of unit types:")
      cat(paste0("  ", nrow(x$causal_types), "\n\n"))
    }

    if (!is.null(attr(x, "restrictions"))) {
      restrictions <- attr(x, "restrictions")
      cat("\nRestrictions:\n")
      for (node in x$nodes) {
        cat(paste0(node,
                   ": ",
                   length(restrictions[[node]]),
                   " restricted types \n\n"))
      }
    }
  } else {

    # dag
    if ("dag" %in% include) {
      cat("\nDag: \n\n")
      print(x$dag)
      cat("\n")
    }

    # nodes
    if ("nodes" %in% include) {
      cat("\nNodes: \n")
      cat(paste(x$nodes, collapse = ", "))
      cat("\n")
    }

    # parents_df
    if ("parents_df" %in% include) {
      cat("\nRoot vs Non-Root status with number and names of parents for each node: \n\n")
      print(x$parents_df)
      cat("\n")
    }

    # parameters
    if ("parameters" %in% include) {
      cat("\nModel parameters with associated probabilities: \n\n")
      cat(names(x$parameters))
      cat("\n")
      cat(x$parameters)
    }

    # parameters_df
    if ("parameters_df" %in% include) {
      cat("\nMapping of model parameters to nodal types: \n")
      # cat("----------------------------------------------------------------\n")
      cat("\n  param_names: name of parameter")
      cat("\n  node: name of endogeneous node associated with the parameter")
      cat("\n  gen: partial causal ordering of the parameter's node")
      cat("\n  param_set: parameter groupings forming a simplex")
      cat("\n  given: if model has confounding gives conditioning nodal type")
      cat("\n  param_value: parameter values")
      cat("\n  priors: hyperparameters of the prior Dirichlet distribution \n\n\n")
      # cat("----------------------------------------------------------------\n\n")
      if (nrow(x$parameters_df) > 10) {
        cat("\n first 10 rows: \n")
        print.data.frame(x[1:10,])
      } else {
        print.data.frame(x$parameters_df)
      }
    }

    # causal_types
    if ("causal_types" %in% include) {
      cat("\nCausal Types: ")
      cat("\n  cartesian product of nodal types\n\n")
      if(nrow(x$causal_types) > 10) {
        cat("\n first 10 causal types: \n")
        print.data.frame(x$causal_types[1:10,])
      } else {
        print.data.frame(x$causal_types)
      }
    }

    # nodal_types
    if ("nodal_types" %in% include) {
      cat("\nNodal types: \n")

      .nodal_types <- x$nodal_types
      nodes <- names(x$nodal_types)

      for (n in nodes) {
        nt <- .nodal_types[[n]]
        interpret <- attr(.nodal_types, "interpret")[[n]]
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

      cat("\nNumber of types by node:\n")

      print(vapply(.nodal_types , length, numeric(1), USE.NAMES = TRUE))

      cat("\n")
    }


    # stan_objects
    if ("stan_objects" %in% include) {
      if (!is.null(x$stan_objects)) print(x$stan_objects)
    }

  }

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
  distribution_summary <- as.data.frame(t(apply(x, 1, summarise_distribution)))
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
  cat("\n")
  cat(x$stan_summary, sep = "\n")
  cat("\n")
  return(invisible(x))
}

