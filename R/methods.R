
#' Print a short summary for a causal model
#'
#' print method for class "\code{causal_model}".
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

  cat("\nCausal statement: \n")
  cat(x$statement)
  cat("\n")

  cat("\nNumber of types by node:\n")
  print(vapply(get_nodal_types(x) , length, numeric(1) ,USE.NAMES = TRUE))


  if (!is.null(x$causal_types)) {
    cat("\nNumber of unit types:")
    cat(paste0(" ", nrow(get_causal_types(x)), "\n"))
  }

  if (!is.null(x$posterior_distribution) & !is.null(x$stan_objects)) {
    cat("\nModel has been updated and contains a posterior distribution with\n")
    cat(paste(x$stan_objects$stan_summary[[2]],"\n"))
    cat("Use inspect(model, 'stan_objects') to inspect stan summary\n\n")
  }

  return(invisible(x))
}

#' Summarizing causal models
#'
#' summary method for class "\code{causal_model}".
#'
#' @param object An object of \code{causal_model} class produced using
#'   \code{make_model} or \code{update_model}.
#' @param ... Further arguments passed to or from other methods.
#'
#' @return Returns the object of class \code{summary.causal_model} that preserves the list structure of \code{causal_model} class and adds the following additional elements:
#' \itemize{
#'   \item \code{"parents"} a list of parents of all nodes in a model,
#'   \item \code{"parameters"} a vector of 'true' parameters,
#'   \item \code{"parameter_names"} a vector of names of parameters,
#'   \item \code{"parameter_mapping"} a matrix mapping from parameters into data types,
#'   \item \code{"parameter_matrix"} a matrix mapping from parameters into causal types,
#'   \item \code{"causal_types"} a data frame listing causal types and the nodal types that produce them,
#'   \item \code{"data_types"} a list with the all data  types consistent with the model; for options see \code{"?get_all_data_types"},
#'   \item \code{"prior_event_probabilities"} a vector of prior data (event) probabilities given a parameter vector; for options see \code{"?get_event_probabilities"},
#'   \item \code{"ambiguities_matrix"} a matrix mapping from causal types into data types,
#'   \item \code{"prior_hyperparameters"} a vector of alpha values used to parameterize Dirichlet prior distributions; optionally provide node names to reduce output \code{"inspect(prior_hyperparameters, c('M', 'Y'))"}
#' }
#'
#' @examples
#' \donttest{
#'
#' model <-
#'   make_model('X -> Y') |>
#'   update_model(
#'     keep_event_probabilities = TRUE,
#'     keep_fit = TRUE,
#'     data = make_data(model, n = 100))
#'
#' summary(model)
#'
#' }
#'
#' @export
summary.causal_model <- function(object, ...) {

  if (class(object) != "causal_model")
    stop("This summary function works with causal_model objects only")

  object$causal_types <- get_causal_types(object)
  object$parents <- get_parents(object)
  object$ambiguities_matrix <- get_ambiguities_matrix(object)
  object$parameters <- get_parameters(object)
  object$parameter_matrix <- get_parameter_matrix(object)

  # Collect '...' into a list
  dots <- list(...)

  # Extract and pass specific arguments to each function
  object$data_types <-
    list(object) |>
    c(get_args_for(get_all_data_types, dots)) |>
    do.call(get_all_data_types, args = _)

  object$prior_event_probabilities <-
    list(object) |>
    c(get_args_for(get_event_probabilities, dots)) |>
    do.call(get_event_probabilities, args = _)

  object$parameter_names <-
    list(object) |>
    c(get_args_for(get_parameter_names, dots)) |>
    do.call(get_parameter_names, args = _)

  object$parameter_mapping <-
    list(object) |>
    c(get_args_for(get_parmap, dots)) |>
    do.call(get_parmap, args = _)

  object$prior_hyperparameters <-
    list(object) |>
    c(get_args_for(get_priors, dots)) |>
    do.call(get_priors, args = _)

  object$prior_distribution <-
    list(object, using = "priors") |>
    c(get_args_for(get_param_dist, dots)) |>
    do.call(get_param_dist, args = _) |>
    suppressMessages()

  object$type_prior <-
    list(object, using = "priors") |>
    c(get_args_for(get_type_prob_multiple, dots)) |>
    do.call(get_type_prob_multiple, args = _)

  object <- structure(object, class = c("summary.causal_model"))

  return(object)
}


#' @rdname summary.causal_model
#'
#' @param x An object of \code{summary.causal_model} class, produced using \code{summary.causal_model}.
#' @param include A character string specifying the objects summaries to print. Defaults to \code{NULL} printing causal statement, specification of nodal types and summary of model restrictions. See \link[=details]{Details} for full list of available values.
#' @param ... Further arguments passed to or from other methods.
#'
#'
#' @details
#'
#' \code{print.summary.causal_model} reports causal statement, full specification of nodal types and summary of model restrictions. By specifying `include` argument users can instead print a custom summary of any set of the following objects contained in the `summary.causal_model`:
#' \itemize{
#'   \item \code{"statement"} A character string giving the causal statement,
#'   \item \code{"nodes"} A list containing the nodes in the model,
#'   \item \code{"parents"} A list of parents of all nodes in a model,
#'   \item \code{"parents_df"} A data frame listing nodes, whether they are root nodes or not, and the number and names of parents they have,
#'   \item \code{"parameters"} A vector of 'true' parameters,
#'   \item \code{"parameters_df"} A data frame containing parameter information,
#'   \item \code{"parameter_names"} A vector of names of parameters,
#'   \item \code{"parameter_mapping"} A matrix mapping from parameters into data types,
#'   \item \code{"parameter_matrix"} A matrix mapping from parameters into causal types,
#'   \item \code{"causal_types"} A data frame listing causal types and the nodal types that produce them,
#'   \item \code{"nodal_types"} A list with the nodal types of the model,
#'   \item \code{"data_types"} A list with the all data types consistent with the model; for options see `"?get_all_data_types"`,
#'   \item \code{"prior_hyperparameters"} A vector of alpha values used to parameterize Dirichlet prior distributions; optionally provide node names to reduce output `inspect(prior_hyperparameters, c('M', 'Y'))`
#'   \item \code{"prior_distribution"} A data frame of the parameter prior distribution,
#'   \item \code{"prior_event_probabilities"} A vector of data (event) probabilities given a single realization of parameters; for options see `"?get_event_probabilities"`,
#'   \item \code{"ambiguities_matrix"} A matrix mapping from causal types into data types,
#'   \item \code{"type_prior"} A matrix of type probabilities using priors,
#'   \item \code{"type_distribution"} A matrix of type probabilities using posteriors,
#'   \item \code{"posterior_distribution"} A data frame of the parameter posterior distribution,
#'   \item \code{"posterior_event_probabilities"} A sample of data (event) probabilities from the posterior,
#'   \item \code{"data"} A data frame with data that was used to update model.
#'   \item \code{"stanfit"} A `stanfit` object generated by Stan,
#'   \item \code{"stan_summary"} A `stanfit` summary with updated parameter names,
#'   \item \code{"stan_objects"} A list of Stan outputs that includes `stanfit`, `data`, and, if requested when updating the model, posterior `event_probabilities` and `type_distribution`,
#' }
#'
#' @examples
#' \donttest{
#' model <-
#'   make_model('X -> Y')
#'
#' print(summary(model), include = "type_distribution")
#' print(summary(model), include = "posterior_distribution")
#' print(summary(model), include = "posterior_event_probabilities")
#' print(summary(model), include = "data_types")
#' print(summary(model), include = "ambiguities_matrix")
#' print(summary(model), include = "prior_hyperparameters")
#'
#' model <-
#'   model |>
#'   update_model(
#'     keep_event_probabilities = TRUE,
#'     keep_fit = TRUE,
#'     data = simulate_data(model, n = 100))
#'
#' print(summary(model), include = c("statement", "nodes"))
#' print(summary(model), include = "parameters_df")
#' print(summary(model), include = "posterior_event_probabilities")
#' print(summary(model), include = "posterior_distribution")
#' print(summary(model), include = "data")
#' print(summary(model), include = "stanfit")
#' print(summary(model), include = "type_distribution")
#' print(summary(model), include = "stan_objects")
#' }
#'
#' @export
print.summary.causal_model <-
  function(x, include = NULL, ... ) {


    wrong <-
      base::setdiff(include, c("statement",
                               "nodes",
                               "parents",
                               "parents_df",
                               "parameters",
                               "parameters_df",
                               "parameter_names",
                               "parameter_mapping",
                               "parameter_matrix",
                               "causal_types",
                               "nodal_types",
                               "data_types",
                               "prior_hyperparameters",
                               "prior_distribution",
                               "prior_event_probabilities",
                               "ambiguities_matrix",
                               "type_prior",
                               "type_distribution",
                               "posterior_distribution",
                               "posterior_event_probabilities",
                               "data",
                               "stanfit",
                               "stan_summary",
                               "stan_objects"))

    if (length(wrong) > 0 & length(wrong) < length(include)) {
      warning("The following requested objects are not supported: ",
              paste0(wrong, collapse = ", "),
              ";\n  printing summary for the available objects only")
    } else if (length(wrong) > 0 & length(wrong) == length(include)) {
      stop("The following requested objects are not supported: ",
           paste0(wrong, collapse = ", "))
    }

    # create dummy messages for updating
    printout <- c()
    printout_upd <- c()

    if (is.null(include)) {
      ## IF INCLUDE IS EMPTY

      # main summary printout
      cat("\nCausal statement: \n")
      cat(x$statement)
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

      cat("Number of types by node:\n")

      print(vapply(nodal_types , length, numeric(1), USE.NAMES = TRUE))

      if (!is.null(x$causal_types)) {
        cat("\nNumber of unit types:")
        cat(paste0("  ", nrow(x$causal_types), "\n"))
      }

      if (!is.null(attr(x, "restrictions"))) {
        restrictions <- attr(x, "restrictions")
        cat("\n\nRestrictions:\n")
        for (node in x$nodes) {
          cat(paste0(node,
                     ": ",
                     length(restrictions[[node]]),
                     " restricted types \n\n"))
        }
      }

      if (!is.null(x$posterior_distribution) & !is.null(x$stan_objects)) {
        cat("\nModel has been updated and contains a posterior distribution with\n")
        cat(paste(x$stan_objects$stan_summary[[2]],"\n"))
        cat("Use inspect(model, 'stan_summary') to inspect stan summary\n")
      }

      if (is.null(x$posterior_distribution) | is.null(x$stan_objects)) {

        printout <-
          c(printout, "posterior_distribution, stan_objects")

      } else {
        if (is.null(x$stan_objects$data)) {

          printout <- c(printout, "specified 'data'")
          printout_upd <- c(printout_upd, "'data'")
        }

        if (is.null(x$stan_objects$event_probabilities)) {

          printout <-
            c(printout, "posterior event_probabilities")
          printout_upd <-
            c(printout_upd, "'keep_event_probabilities = TRUE'")

        }

        if (is.null(x$stan_objects$type_distribution)) {

          printout <-
            c(printout, "type_distribution")
          printout_upd <-
            c(printout_upd, "'type_distribution = TRUE'")

        }

        if (is.null(x$stan_objects$stanfit)) {

          printout <-
            c(printout, "stanfit")
          printout_upd <-
            c(printout_upd, "'keep_fit = TRUE'")

        }
      }

      if (length(printout) > 0) {
        if (length(printout_upd) > 0) {
          message(
            paste0("Model does not contain the following objects: ",
                   paste0(unique(printout), collapse = ", "),
                   ";\n  to include these objects update model with ",
                   paste0(unique(printout_upd), collapse = ", "))
          )
        } else {
          message(
            paste0("Model does not contain: ",
                   paste0(unique(printout), collapse = ", "),
                   ";\n  to include these objects update model")
          )
        }
      }


    } else {
      ## IF INCLUDE IS SPECIFIED

      # statement
      if ("statement" %in% include) {
        cat("\nCausal statement: \n")
        cat(x$statement)
        cat("\n")
      }

      # nodes
      if ("nodes" %in% include) {
        cat("\nNodes: \n")
        cat(paste(x$nodes, collapse = ", "))
        cat("\n")
      }

      if ("parents" %in% include) {
        cat("\nParents: \n\n")
        nodes <- names(x$parents)

        for (n in nodes) {
          n_parents <- x$parents[[n]]
          cat(paste0("$", n, "\n"))
          if(length(n_parents)==0)
            cat("Node has no parents \n")
          else
            cat(paste(n_parents, sep = " "))
          cat("\n" )
        }
      }

      # parents_df
      if ("parents_df" %in% include) {
        cat("\nRoot vs Non-Root status with number and names of parents for each node: \n\n")
        print(x$parents_df)
      }

      # parameters
      if ("parameters" %in% include) {
        cat("\nModel parameters with associated probabilities: \n\n")
        cat(names(x$parameters))
        cat("\n")
        cat(x$parameters)
        cat("\n")
      }

      # parameters_df
      if ("parameters_df" %in% include) {
        cat("\nMapping of model parameters to nodal types: \n")
        # cat("----------------------------------------------------------------\n")
        cat("\n  param_names: name of parameter")
        cat("\n  node:        name of endogeneous node associated")
        cat("\n               with the parameter")
        cat("\n  gen:         partial causal ordering of the")
        cat("\n               parameter's node")
        cat("\n  param_set:   parameter groupings forming a simplex")
        cat("\n  given:       if model has confounding gives")
        cat("\n               conditioning nodal type")
        cat("\n  param_value: parameter values")
        cat("\n  priors:      hyperparameters of the prior")
        cat("\n               Dirichlet distribution \n\n")
        # cat("----------------------------------------------------------------\n\n")
        if (nrow(x$parameters_df) > 10) {
          cat("\n first 10 rows: \n")
          print.data.frame(x[1:10,])
        } else {
          print.data.frame(x$parameters_df)
        }
      }

      # parameter_names
      if ("parameter_names" %in% include) {
        cat("\nParameter names: \n")
        cat(x$parameter_names, sep = ", ")
        cat("\n")
      }

      # parameter_matrix
      if ("parameter_matrix" %in% include) {
        cat(paste0("\nParameter matrix:\n"))
        cat(paste0("\n  rows:   parameters"))
        cat(paste0("\n  cols:   causal types"))
        cat(paste0("\n  cells:  whether a parameter probability is used"))
        cat(paste0("\n          in the calculation of causal type probability\n\n"))

        print(x$parameter_matrix)
        if (!is.null(attr(x, "param_set"))) {
          cat("\n")
          param_set <- attr(x, "param_set")
          cat("\n param_set  (P)\n ")
          cat(paste0(param_set, collapse = "  "))
        }
      }

      # parameter_mapping
      if ("parameter_mapping" %in% include) {
        cat("\nParameter mapping matrix: \n")
        cat("\n  Maps from parameters to data types, with")
        cat("\n  possibly multiple columns for each data type")
        cat("\n  in cases with confounding. \n\n")
        print(data.frame(x$parameter_mapping))
      }

      # causal_types
      if ("causal_types" %in% include) {
        cat("\nCausal Types:\n")
        cat("\n  Cartesian product of nodal types\n\n")
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
        nodes <- names(.nodal_types)

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

      }

      # data_types
      if ("data_types" %in% include) {
        cat("\nData frame of all possible data (events) given the model:\n\n")
        print(x$data_types)
      }

      # ambiguities_matrix
      if ("ambiguities_matrix" %in% include) {
        cat("\nMapping from causal types into data types:\n\n")
        print(x$ambiguities_matrix)
      }

      # prior_event_probabilties
      if ("prior_event_probabilities" %in% include) {
        cat("\nProbabilities of observing data (events)")
        cat("\nfor a given set of parameter values:\n\n")
        print(data.frame(event_probs = x$prior_event_probabilities))
      }

      # prior_hyperparameters
      if ("prior_hyperparameters" %in% include) {
        cat("\nAlpha parameter values used for Dirichlet prior distributions:\n\n")
        print(x$prior_hyperparameters)
      }

      # prior_distribution
      if ("prior_distribution" %in% include) {
        cat("\nSummary statistics of model parameters prior distributions:\n")
        cat(paste(
          "\n  Distributions matrix dimensions are",
          "\n ", dim(x$prior_distribution)[1], "rows (draws) by",
          dim(x$prior_distribution)[2], "cols (parameters)\n\n", sep = " "))
        distribution_summary <-
          as.data.frame(t(apply(x$prior_distribution, 2,
                                summarise_distribution)))
        rounding_threshold <- find_rounding_threshold(distribution_summary)
        print.data.frame(round(distribution_summary, rounding_threshold))
      }

      # type_prior
      if ("type_prior" %in% include) {
        cat("\nSummary statistics of causal types prior distributions:\n")
        cat(paste(
          "\n  Distributions matrix dimensions are",
          "\n ", dim(x$type_prior)[1], "rows (causal types) by",
          dim(x$type_prior)[2], "cols (draws)\n\n", sep = " "))
        distribution_summary <-
          # why is this the case
          as.data.frame(t(apply(x$type_prior, 1, summarise_distribution)))
        rounding_threshold <- find_rounding_threshold(distribution_summary)
        print.data.frame(round(distribution_summary, rounding_threshold))
      }

      # type_distribution
      if ("type_distribution" %in% include) {
        if (!is.null(x$stan_objects$type_distribution)) {
          cat("\nPosterior draws of causal types (transformed parameters):\n")
          cat(paste(
            "\n  Distributions matrix dimensions are",
            "\n ", dim(x$stan_objects$type_distribution)[1], "rows (draws) by",
            dim(x$stan_objects$type_distribution)[2], "cols (causal types)\n\n", sep = " "))
          distribution_summary <-
            as.data.frame(t(apply(x$stan_objects$type_distribution, 2,
                                  summarise_distribution)))
          rounding_threshold <- find_rounding_threshold(distribution_summary)
          print.data.frame(round(distribution_summary, rounding_threshold))
        } else {
          printout <- c(printout,
                        "posterior type_distribution")
          printout_upd <- c(printout_upd,
                            "'type_distribution = TRUE'")
        }
      }

      # posterior_distribution
      if ("posterior_distribution" %in% include) {
        if (!is.null(x$posterior_distribution)) {
          cat("\nSummary statistics of model parameters posterior distributions:\n")
          cat(paste(
            "\n  Distributions matrix dimensions are",
            "\n ", dim(x$posterior_distribution)[1], "rows (draws) by",
            dim(x$posterior_distribution)[2], "cols (parameters)\n\n", sep = " "))
          distribution_summary <-
            as.data.frame(t(apply(x$posterior_distribution, 2,
                                  summarise_distribution)))
          rounding_threshold <- find_rounding_threshold(distribution_summary)
          print.data.frame(round(distribution_summary, rounding_threshold))

        } else {
          printout <- c(printout,
                        "posterior_distribution")
          # printout_upd <- c(printout_upd, "")
        }
      }

      # posterior_event_probabilities
      if ("posterior_event_probabilities" %in% include) {
        if (!is.null(x$stan_objects$event_probabilities)) {
          cat("\nPosterior draws of event probabilities (transformed parameters):\n")
          cat(paste(
            "\n  Distributions matrix dimensions are",
            "\n ", dim(x$stan_objects$event_probabilities)[1], "rows (draws) by",
            dim(x$stan_objects$event_probabilities)[2], "cols (events)\n\n", sep = " "))
          distribution_summary <-
            as.data.frame(t(apply(x$stan_objects$event_probabilities, 2,
                                  summarise_distribution)))
          rounding_threshold <- find_rounding_threshold(distribution_summary)
          print.data.frame(round(distribution_summary, rounding_threshold))
        } else {
          printout <- c(printout,
                        "event_probabilities")
          printout_upd <- c(printout_upd,
                            "'keep_event_probabilities = TRUE'")
        }
      }

      # data
      if ("data" %in% include) {
        if (!is.null(x$stan_objects$data)) {
          cat("\nData used to update the model:\n")
          cat(paste(
            "\n  Data frame dimensions are",
            "\n ", dim(x$stan_objects$data)[1], "rows by",
            dim(x$stan_objects$data)[2], "cols\n\n", sep = " "))

          if (nrow(x$stan_objects$data) > 10) {
            cat("first 10 rows: \n")
            print.data.frame(x$stan_objects$data[1:10,])
          } else {
            print.data.frame(x$stan_objects$data)
          }
        } else {
          printout <- c(printout, "specified 'data'")
          printout_upd <- c(printout_upd, "'data'")
        }
      }

      # stan_objects
      if ("stanfit" %in% include) {
        if (!is.null(x$stan_objects$stanfit)) {
          cat("\nStan model summary:\n")
          print(x$stan_objects)
        } else {
          printout <- c(printout, "stanfit")
          printout_upd <- c(printout_upd, "'keep_fit = TRUE'")
        }
      }

      # stan_objects
      if ("stan_summary" %in% include | "stan_objects" %in% include) {
        if (!is.null(x$stan_objects)) {
          cat("\nStan model summary:\n")
          print(x$stan_objects)
        } else {
          printout <- c(printout, "stan_objects")
          # printout_upd <- c(printout_upd, "")
        }
      }

      if (length(printout) > 0) {
        if (length(printout_upd) > 0) {
          stop(
            paste0("Model does not contain the following requested objects: ",
                   paste0(unique(printout), collapse = ", "),
                   ";\n  to include these objects update model with ",
                   paste0(unique(printout_upd), collapse = ", "))
          )
        } else {
          stop(
            paste0("Model does not contain: ",
                   paste0(unique(printout), collapse = ", "),
                   ";\n  to include these objects update model")
          )
        }
      }

    }

    cat("\n")

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
  return(invisible(x))
}

