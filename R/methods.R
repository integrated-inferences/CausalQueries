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
#' causal relations using \code{dagitty} syntax,
#' number of nodal types per parent in a DAG, and number of causal types.
#'
#' @export
print.causal_model <- function(x, ...) {

  nnt <- vapply(get_nodal_types(x), length, numeric(1), USE.NAMES = TRUE)

  cat("\nCausal statement: \n")
  cat(x$statement)
  cat("\n")

  cat("\nNumber of nodal types by node:\n")
  print(nnt)


  if (!is.null(x$causal_types)) {
    cat("\nNumber of causal types:")
    cat(paste0(" ", prod(nnt), "\n"))
  }

  if (!is.null(x$posterior_distribution) & !is.null(x$stan_objects)) {
    cat("\nModel has been updated and contains a posterior distribution with\n")
    cat(paste(x$stan_objects$stan_summary[[2]], "\n"))
    cat("Use inspect(model, 'stan_objects') to inspect stan summary\n\n")

    if(x$stan_objects$stan_warnings != "") {
      cat("Warnings passed from rstan during updating:\n")
      cat(x$stan_objects$stan_warnings)
    }
  }

  return(invisible(x))
}

#' Summarizing causal models
#'
#' summary method for class "\code{causal_model}".
#'
#' @param object An object of \code{causal_model} class produced using
#'   \code{make_model} or \code{update_model}.
#' @param include A character string specifying the additional objects to include in summary. Defaults to \code{NULL}. See details for full list of available values.
#' @param ... Further arguments passed to or from other methods.
#'
#' @return Returns the object of class \code{summary.causal_model} that preserves the list structure of \code{causal_model} class and adds the following additional objects:
#' \itemize{
#'   \item \code{"parents"} a list of parents of all nodes in a model,
#'   \item \code{"parameters"} a vector of 'true' parameters,
#'   \item \code{"parameter_names"} a vector of names of parameters,
#'   \item \code{"data_types"} a list with the all data  types consistent with the model; for options see \code{"?get_all_data_types"},
#'   \item \code{"prior_event_probabilities"} a vector of prior data (event) probabilities given a parameter vector; for options see \code{"?get_event_probabilities"},
#'   \item \code{"prior_hyperparameters"} a vector of alpha values used to parameterize Dirichlet prior distributions; optionally provide node names to reduce output \code{"inspect(prior_hyperparameters, c('M', 'Y'))"}
#' }
#'
#' @details
#' In addition to the default objects included in `summary.causal_model` users can request additional objects via `include` argument. Note that these additional objects can be large for complex models and can increase computing time. The `include` argument can be a vector of any of the following additional objects:
#' \itemize{
#'   \item \code{"parameter_matrix"} A matrix mapping from parameters into causal types,
#'   \item \code{"parameter_mapping"} a matrix mapping from parameters into data types,
#'   \item \code{"causal_types"} A data frame listing causal types and the nodal types that produce them,
#'   \item \code{"prior_distribution"} A data frame of the parameter prior distribution,
#'   \item \code{"ambiguities_matrix"} A matrix mapping from causal types into data types,
#'   \item \code{"type_prior"} A matrix of type probabilities using priors.
#' }
#'
#' @examples
#' \donttest{
#' model <-
#'   make_model("X -> Y")
#'
#' model |>
#'   update_model(
#'     keep_event_probabilities = TRUE,
#'     keep_fit = TRUE,
#'     data = make_data(model, n = 100)
#'   ) |>
#'   summary()
#' }
#'
#' @export
summary.causal_model <- function(object, include = NULL, ...) {
  is_a_model(object)

  object$parents <- get_parents(object)
  object$parameters <- get_parameters(object)

  # Collect '...' into a list
  dots <- list(...)

  # Extract and pass specific arguments to each function
  object$data_types <-
    list(object) |>
    c(get_args_for(get_all_data_types, dots)) |>
    do.call(get_all_data_types, args = _)

  object$parameter_names <-
    list(object) |>
    c(get_args_for(get_parameter_names, dots)) |>
    do.call(get_parameter_names, args = _)

  object$prior_hyperparameters <-
    list(object) |>
    c(get_args_for(get_priors, dots)) |>
    do.call(get_priors, args = _)

  if (!is.null(include)) {
    # Only below values admissible to add to summary or to pass to printing
    wrong <-
      base::setdiff(include, c(
        "statement",
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
        "stan_warnings",
        "stan_summary",
        "stan_xs"
      ))

    if (length(wrong) > 0 & length(wrong) <= length(include)) {
      stop(
        "The following requested objects are not supported: ",
        paste0(wrong, collapse = ", ")
      )
    }

    if ("prior_event_probabilities" %in% include) {
      object$prior_event_probabilities <-
      list(object) |>
      c(get_args_for(get_event_probabilities, dots)) |>
      do.call(get_event_probabilities, args = _)
    }

    if ("parameter_mapping" %in% include) {
    object$parameter_mapping <-
      list(object) |>
      c(get_args_for(get_parmap, dots)) |>
      do.call(get_parmap, args = _)
    }

    if ("parameter_matrix" %in% include) {
      object$parameter_matrix <- get_parameter_matrix(object)
    }

    if ("causal_types" %in% include) {
      object$causal_types <- get_causal_types(object)
    }

    if ("prior_distribution" %in% include) {
      object$prior_distribution <-
        list(object, using = "priors") |>
        c(get_args_for(get_param_dist, dots)) |>
        do.call(get_param_dist, args = _) |>
        suppressMessages()
    }

    if ("ambiguities_matrix" %in% include) {
      object$ambiguities_matrix <- get_ambiguities_matrix(object)
    }

    if ("type_prior" %in% include) {
      object$type_prior <-
        list(object, using = "priors") |>
        c(get_args_for(get_type_prob_multiple, dots)) |>
        do.call(get_type_prob_multiple, args = _)
    }
  }

  object <- structure(object, class = c("summary.causal_model"))
  attr(object, "include") <- include

  return(object)
}


#' @rdname summary.causal_model
#'
#' @param x An object of \code{summary.causal_model} class, produced using \code{summary.causal_model}.
#' @param what A character string specifying the objects summaries to print. Defaults to \code{NULL} printing causal statement, specification of nodal types and summary of model restrictions. See details for full list of available values.
#' @param ... Further arguments passed to or from other methods.
#'
#'
#' @details
#' \code{print.summary.causal_model} reports causal statement, full specification of nodal types and summary of model restrictions. By specifying `what` argument users can instead print a custom summary of any set of the following objects contained in the `summary.causal_model`:
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
#'   \item \code{"prior_event_probabilities"} A vector of data (event) probabilities given a single (sepcified) parameter vector; for options see `"?get_event_probabilities"`,
#'   \item \code{"ambiguities_matrix"} A matrix mapping from causal types into data types,
#'   \item \code{"type_prior"} A matrix of type probabilities using priors,
#'   \item \code{"type_distribution"} A matrix of type probabilities using posteriors,
#'   \item \code{"posterior_distribution"} A data frame of the parameter posterior distribution,
#'   \item \code{"posterior_event_probabilities"} A sample of data (event) probabilities from the posterior,
#'   \item \code{"data"} A data frame with data that was used to update model.
#'   \item \code{"stanfit"} A `stanfit` object generated by Stan,
#'   \item \code{"stan_summary"} A `stanfit` summary with updated parameter names,
#'   \item \code{"stan_objects"} A list of Stan outputs that includes `stanfit`, `data`, and, if requested when updating the model, posterior `event_probabilities` and `type_distribution`.
#' }
#'
#' @examples
#' \donttest{
#' model <-
#'   make_model("X -> Y") |>
#'   update_model(
#'     keep_event_probabilities = TRUE,
#'     keep_fit = TRUE,
#'     data = make_data(model, n = 100)
#'   )
#'
#' print(summary(model), what = "type_distribution")
#' print(summary(model), what = "posterior_distribution")
#' print(summary(model), what = "posterior_event_probabilities")
#' print(summary(model), what = "data_types")
#' print(summary(model), what = "ambiguities_matrix")
#' print(summary(model), what = "prior_hyperparameters")
#' print(summary(model), what = c("statement", "nodes"))
#' print(summary(model), what = "parameters_df")
#' print(summary(model), what = "posterior_event_probabilities")
#' print(summary(model), what = "posterior_distribution")
#' print(summary(model), what = "data")
#' print(summary(model), what = "stanfit")
#' print(summary(model), what = "type_distribution")
#' }
#'
#' @export
print.summary.causal_model <-
  function(x, what = NULL, ...) {

    note_1 <- NA
    short <- !is.null(what)

    # general printing requests: provided whenever 'what' is not specified
    if (is.null(what)) {

      # create dummy messages for updating
      printout <- c()
      printout_upd <- c()

      ## IF WHAT IS EMPTY

      # main summary printout: statement
      cat("\nCausal statement: \n")
      cat(x$statement)
      cat("\n")

      # main summary printout: nodal types
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

      print(vapply(nodal_types, length, numeric(1), USE.NAMES = TRUE))

      if (!is.null(x$causal_types)) {
        cat("\nNumber of causal types:")
        cat(paste0("  ", nrow(x$causal_types), "\n"))
      }

      if (!is.null(attr(x, "restrictions"))) {
        restrictions <- attr(x, "restrictions")
        cat("\n\nRestrictions:\n")
        for (node in x$nodes) {
          cat(paste0(
            node,
            ": ",
            length(restrictions[[node]]),
            " restricted types \n\n"
          ))
        }
      }

      if (!is.null(x$posterior_distribution) & !is.null(x$stan_objects)) {
        cat("\nModel has been updated and contains a posterior distribution with\n")
        cat(paste(x$stan_objects$stan_summary[[2]], "\n"))
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
        note_1 <- {
          if (length(printout_upd) > 0) {
            (
              paste0(
                "\nNote: Model does not contain the following objects: ",
                paste0(unique(printout), collapse = ", "),
                ";\nto include these objects update model with ",
                paste0(unique(printout_upd), collapse = ", "),
                "\n"
              )
            )
          } else {
            (
              paste0(
                "\nNote: Model does not contain: ",
                paste0(unique(printout), collapse = ", "),
                ";\nto include these objects use update_model()\n"
              )
            )
          }
        }
      }
    }

    # specific printing requests:

    # pass on any additional elements to what from include argument in summary
    what <- c(what, attr(x, "include"))

    # avoid any repetition in long summaries
    if(!short)
    what <- base::setdiff(what, c("statement", "nodal_types"))



    if(!is.null(what)) {

      # create dummy messages for updating
      printout <- c()
      printout_upd <- c()


      ## IF WHAT IS SPECIFIED

      wrong <-
        base::setdiff(what, c(
          "statement",
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
          "stan_warnings",
          "stan_summary",
          "stan_objects"
        ))

      if (length(wrong) > 0 & length(wrong) <= length(what)) {
        stop(
          "The following requested objects are not supported: ",
          paste0(wrong, collapse = ", ")
        )
      }

      # statement
      if ("statement" %in% what) {
        cat("\nCausal statement: \n")
        cat(x$statement)
        cat("\n")
      }

      # nodes
      if ("nodes" %in% what) {
        cat("\nNodes: \n")
        cat(paste(x$nodes, collapse = ", "))
        cat("\n")
      }

      if ("parents" %in% what) {
        cat("\nParents: \n\n")
        nodes <- names(x$parents)

        for (n in nodes) {
          n_parents <- x$parents[[n]]
          cat(paste0("$", n, "\n"))
          if (length(n_parents) == 0) {
            cat("Node has no parents \n")
          } else {
            cat(paste(n_parents, sep = " "))
          }
          cat("\n")
        }
      }

      # parents_df
      if ("parents_df" %in% what) {
        cat("\nRoot vs Non-Root status with number and names of parents for each node: \n\n")
        print(x$parents_df)
      }

      # parameters
      if ("parameters" %in% what) {
        cat("\nparameters\nModel parameters with associated probabilities: \n\n")
        print(x$parameters)
      }


      # parameters_df
      if ("parameters_df" %in% what) {
        cat("\nparameters_df\nMapping of model parameters to nodal types: \n")
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

        snippet(x$parameters_df)

      }

      # parameter_names
      if ("parameter_names" %in% what) {
        cat("\nParameter names: \n")
        cat(x$parameter_names, sep = ", ")
        cat("\n")
      }

      # parameter_matrix
      if (("parameter_matrix" %in% what) & !is.null(x$parameter_matrix)) {
        cat(paste0("\nparameter_matrix:\n"))
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
      } else if (("parameter_matrix" %in% what) & is.null(x$parameter_matrix)) {
        warning("Model summary does not contain parameter_matrix; to include this object use summary with 'include = 'parameter_matrix''")
      }

      # parameter_mapping
      if (("parameter_mapping" %in% what) & !is.null(x$parameter_mapping)) {
        cat("\nparameter_mapping (Parameter mapping matrix) \n")
        cat("\n  Maps from parameters to data types, with")
        cat("\n  possibly multiple columns for each data type")
        cat("\n  in cases with confounding. \n\n")
        snippet(data.frame(x$parameter_mapping))
      } else if (("parameter_mapping" %in% what) & is.null(x$parameter_mapping)) {
        warning("Model summary does not contain parameter_mapping; to include this object use summary with 'include = 'parameter_mapping''")
      }

      # causal_types
      if ("causal_types" %in% what & !is.null(x$causal_types)) {
        cat("\ncausal_types (Causal Types)\n")
        cat("\nCartesian product of nodal types\n")
        snippet(x$causal_types)

      } else if ("causal_types" %in% what & is.null(x$causal_types)) {
        warning("Model summary does not contain causal_types; to include this object use summary with 'include = 'causal_types''")
      }

      # nodal_types
      if ("nodal_types" %in% what) {
        cat("\nnodal_types (Nodal types): \n")

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

        print(vapply(.nodal_types, length, numeric(1), USE.NAMES = TRUE))
      }

      # data_types
      if ("data_types" %in% what) {
        cat("\ndata_types (Data types):\nData frame of all possible data (events) given the model:\n\n")
        print(x$data_types)
      }

      # ambiguities_matrix
      if (("ambiguities_matrix" %in% what) & !is.null(x$ambiguities_matrix)) {
        cat("\nambiguities_matrix (Ambiguities matrix)\nMapping from causal types into data types:\n")

        snippet(data.frame(x$ambiguities_matrix))

      } else if (("ambiguities_matrix" %in% what) & is.null(x$ambiguities_matrix)) {
        warning("Model summary does not contain ambiguities_matrix; to include this object use summary with 'include = 'ambiguities_matrix''")
      }

      # prior_event_probabilties
      if ("prior_event_probabilities" %in% what) {
        cat("\nprior_event_probabilities\nProbabilities of observing data (events)")
        cat("\nfor a specified set of parameter values:\n\n")
        print(data.frame(event_probs = x$prior_event_probabilities))
      }

      # prior_hyperparameters
      if ("prior_hyperparameters" %in% what) {
        cat("\nprior_hyperparameters\nAlpha parameter values used for Dirichlet prior distributions:\n\n")
        print(x$prior_hyperparameters)
      }

      # prior_distribution
      if (("prior_distribution" %in% what) & !is.null(x$prior_distribution)) {
        cat("\nprior_distribution\nSummary statistics of model parameters prior distributions:\n")
        cat(paste(
          "\n  Distributions matrix dimensions are",
          "\n ", dim(x$prior_distribution)[1], "rows (draws) by",
          dim(x$prior_distribution)[2], "cols (parameters)\n\n",
          sep = " "
        ))
        distribution_summary <-
          as.data.frame(t(apply(
            x$prior_distribution, 2,
            summarise_distribution
          )))
        rounding_threshold <- find_rounding_threshold(distribution_summary)
        print.data.frame(round(distribution_summary, rounding_threshold))
      } else if (("prior_distribution" %in% what) & is.null(x$prior_distribution)) {
        warning("Model summary does not contain prior_distribution; to include this object use summary with 'include = 'prior_distribution''")
      }

      # type_prior
      if (("type_prior" %in% what) & !is.null(x$type_prior)) {
        cat("\ntype_prior\nSummary statistics of causal types prior distributions:\n")
        cat(paste(
          "\n  Distributions matrix dimensions are",
          "\n ", dim(x$type_prior)[1], "rows (causal types) by",
          dim(x$type_prior)[2], "cols (draws)\n\n",
          sep = " "
        ))
        distribution_summary <-
          # why is this the case
          as.data.frame(t(apply(x$type_prior, 1, summarise_distribution)))
        rounding_threshold <- find_rounding_threshold(distribution_summary)
        print.data.frame(round(distribution_summary, rounding_threshold))
      } else if (("type_prior" %in% what) & is.null(x$type_prior)) {
        warning("Model summary does not contain type_prior; to include this object use summary with 'include = 'type_prior''")
      }

      # type_distribution
      if ("type_distribution" %in% what) {
        if (!is.null(x$stan_objects$type_distribution)) {
          cat("\ntype_distribution\nPosterior draws of causal types (transformed parameters):\n")
          cat(paste(
            "\n  Distributions matrix dimensions are",
            "\n ", dim(x$stan_objects$type_distribution)[1], "rows (draws) by",
            dim(x$stan_objects$type_distribution)[2], "cols (causal types)\n\n",
            sep = " "
          ))
          distribution_summary <-
            as.data.frame(t(apply(
              x$stan_objects$type_distribution, 2,
              summarise_distribution
            )))
          rounding_threshold <- find_rounding_threshold(distribution_summary)
          print.data.frame(round(distribution_summary, rounding_threshold))
        } else {
          printout <- c(
            printout,
            "posterior type_distribution"
          )
          printout_upd <- c(
            printout_upd,
            "'type_distribution = TRUE'"
          )
        }
      }

      # posterior_distribution
      if ("posterior_distribution" %in% what) {
        if (!is.null(x$posterior_distribution)) {
          cat("\nposterior_distribution\nSummary statistics of model parameters posterior distributions:\n")
          cat(paste(
            "\n  Distributions matrix dimensions are",
            "\n ", dim(x$posterior_distribution)[1], "rows (draws) by",
            dim(x$posterior_distribution)[2], "cols (parameters)\n\n",
            sep = " "
          ))
          distribution_summary <-
            as.data.frame(t(apply(
              x$posterior_distribution, 2,
              summarise_distribution
            )))
          rounding_threshold <- find_rounding_threshold(distribution_summary)
          print.data.frame(round(distribution_summary, rounding_threshold))
        } else {
          printout <- c(
            printout,
            "posterior_distribution"
          )
          # printout_upd <- c(printout_upd, "")
        }
      }

      # posterior_event_probabilities
      if ("posterior_event_probabilities" %in% what) {
        if (!is.null(x$stan_objects$event_probabilities)) {
          cat("\nposterior_event_probabilities\nPosterior draws of event probabilities (transformed parameters):\n")
          cat(paste(
            "\n  Distributions matrix dimensions are",
            "\n ", dim(x$stan_objects$event_probabilities)[1], "rows (draws) by",
            dim(x$stan_objects$event_probabilities)[2], "cols (events)\n\n",
            sep = " "
          ))
          distribution_summary <-
            as.data.frame(t(apply(
              x$stan_objects$event_probabilities, 2,
              summarise_distribution
            )))
          rounding_threshold <- find_rounding_threshold(distribution_summary)
          print.data.frame(round(distribution_summary, rounding_threshold))
        } else {
          printout <- c(
            printout,
            "event_probabilities"
          )
          printout_upd <- c(
            printout_upd,
            "'keep_event_probabilities = TRUE'"
          )
        }
      }

      # data
      if ("data" %in% what) {
        if (!is.null(x$stan_objects$data)) {
          cat("\nData used to update the model:\n")
          cat(paste(
            "\ndata\n  Data frame dimensions are",
            "\n ", dim(x$stan_objects$data)[1], "rows by",
            dim(x$stan_objects$data)[2], "cols\n\n",
            sep = " "
          ))

          snippet(x$stan_objects$data)

        } else {
          printout <- c(printout, "specified 'data'")
          printout_upd <- c(printout_upd, "'data'")
        }
      }

      # stan_objects: stanfit
      if ("stanfit" %in% what) {
        if (!is.null(x$stan_objects$stanfit)) {
          cat("\nstanfit\nStan model summary:\n")
          print(x$stan_objects$stanfit)
        } else {
          printout <- c(printout, "stanfit")
          printout_upd <- c(printout_upd, "'keep_fit = TRUE'")
        }
      }

      # stan_objects: messages
      if ("stan_warnings" %in% what) {
        if (!is.null(x$stan_objects$stan_warnings)) {
          cat("\nstan_warnings\nStan warnings generated during updating:\n")
          cat(x$stan_objects$stan_warnings)
        } else {
          printout <- c(printout, "stan_warnings")
          printout_upd <- c(printout_upd, "'keep_fit = TRUE'")
        }
      }


      # stan_objects
      if ("stan_summary" %in% what | "stan_objects" %in% what) {
        if (!is.null(x$stan_objects)) {
          cat("\nstan_summary\nStan model summary:\n")
          print(x$stan_objects)
        } else {
          printout <- c(printout, "stan_objects")
          # printout_upd <- c(printout_upd, "")
        }
      }



      if (length(printout) > 0) {
        if (length(printout_upd) > 0) {
          stop(
            paste0(
              "Model does not contain the following requested objects: ",
              paste0(unique(printout), collapse = ", "),
              ";\n  to include these objects update model with ",
              paste0(unique(printout_upd), collapse = ", ")
            )
          )
        } else {
          stop(
            paste0(
              "Model does not contain: ",
              paste0(unique(printout), collapse = ", "),
              ";\n  to include these objects update model"
            )
          )
        }
      }
    }

    # Add notes if not short summary
    if(!short) {

      if(!is.na(note_1)) cat(note_1)
      cat("\n")
      cat("Note: To pose causal queries of this model use query_model()\n")
      cat("\n")

      if(!is.null(x$posterior_distribution)){
        if(x$stan_objects$stan_warnings != "") {
          cat("Note: warnings passed from rstan during updating:\n")
          cat(x$stan_objects$stan_warnings)
        }}

    }


    return(invisible(x))
  }

#' helper to print snippets of large objects
#' @noRd
#' @keywords internal

snippet <- function(df, nc = 10, nr = 10) {
  if (nrow(df) > nr | ncol(df) > nc) {
    cat(paste0("\nsnippet (use grab() to access full ", nrow(df), " x ", ncol(df), " object): \n\n"))
    print.data.frame(df[1:(min(nrow(df), nr)), 1:(min(ncol(df), nc))])
  } else {
    print.data.frame(df)
  }
}


#' Print a tightened summary of model queries
#'
#' print method for class \code{model_query}.
#'
#' @param x An object of \code{model_query} class.
#' @param ... Further arguments passed to or from other methods.
#'
#' @rdname print.model_query
#' @export
#'
print.model_query <- function(x, ...) {
  cred.low <- NULL
  cred.high <- NULL
  case_level <- NULL
  given <- NULL

  if(any("posteriors" %in% x$using)){
    if(any(attr(x, "stan_warnings") != "")) {
      cat("Note: warnings passed from rstan during updating:\n")
      cat(attr(x, "stan_warnings"))
      cat("\n")
    }}

  text1 <- "Causal queries generated by query_model"

  # Output simplification
  if (all(c("using", "case_level", "sd", "given") %in% names(x))) {
    if (all(x$using == "parameters" | x$case_level)) {
      x <- x |>
        dplyr::select(-sd, -cred.low, -cred.high)
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




#' Summarizing model queries
#'
#' summary method for class "\code{model_query}".
#'
#' @param object An object of \code{model_query} class produced using
#'   \code{query_model}
#' @param ... Further arguments passed to or from other methods.
#'
#' @return Returns the object of class \code{summary.model_query}
#'
#' @examples
#' \donttest{
#' model <-
#'   make_model("X -> Y") |>
#'   query_model("Y[X=1] > Y[X=1]")  |>
#'   summary()
#'}
#'
#' @export
summary.model_query <- function(object, ...) {
  print_call_and_date(object)
  print.model_query(object)
}


#' @rdname summary.model_query
#'
#' @param x an object of \code{model_query} class produced using
#'  \code{query_model}
#' @param ... Further arguments passed to or from other methods.
#'
#' @export

print.summary.model_query <- function(x, ...) {
  print_call_and_date(x)
  print.model_query(x)
}

# Helper function to print the call and date attributes
#'
#' @noRd
#' @keywords internal
print_call_and_date <- function(object) {
  cat("Call: \n")
  cat(paste(deparse(attr(object, "call")), collapse = " "), "\n")

  cat("\nQueries evaluated on: \n")
  cat(attr(object, "date"))
  cat("\n")
}


#' Print a short summary for stan_objects
#'
#' print method for class \code{stan_objects}.
#'
#' @param x An object of the \code{stan_objects} class, which is a list
#' containing the data, \code{type_distribution} and  \code{stan_summary}.
#' @param ... Further arguments passed to or from other methods.
#'
#' @noRd
#' @keywords internal
print.stan_objects <- function(x, ...) {
  cat("\n")
  cat(x$stan_summary, sep = "\n")
  return(invisible(x))
}
