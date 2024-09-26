#' Realise outcomes
#'
#' Realise outcomes for all causal types. Calculated by sequentially
#' calculating endogenous nodes. If a do operator is applied to any node then
#' it takes the given value and all its descendants are generated accordingly.
#'
#' If a node is not specified all outcomes are realised for all possible
#' causal types consistent with the model. If a node is specified then outcomes
#' of Y are returned conditional on different values of parents, whether or
#' not these values of the parents obtain given restrictions under the model.
#'
#' @details \code{realise_outcomes} starts off by creating types
#'   (via \code{get_nodal_types}). It then takes types of endogenous
#'   and reveals their outcome based on the value that their parents took.
#'   Exogenous nodes outcomes correspond to their type.
#' @inheritParams CausalQueries_internal_inherit_params
#' @param dos A named \code{list}. Do actions defining node values,
#'   e.g., \code{list(X = 0, M = 1)}.
#' @param node A character. An optional quoted name of the node whose
#'   outcome should be revealed. If specified all values of parents need
#'   to be specified via \code{dos}.
#' @param add_rownames logical indicating whether to add causal types
#'   as rownames to the output
#' @return A \code{data.frame} object of revealed data for each node (columns)
#'   given causal / nodal type (rows) .
#' @keywords internal
#' @examples
#' \donttest{
#' make_model("X -> Y") |>
#'   realise_outcomes()
#'
#' make_model("X -> Y <- W") |>
#' set_restrictions(labels = list(X = "1", Y="0010"), keep = TRUE) |>
#'  realise_outcomes()
#'
#' make_model("X1->Y; X2->M; M->Y") |>
#' realise_outcomes(dos = list(X1 = 1, M = 0))
#'
#' # With node specified
#' make_model("X->M->Y") |>
#' realise_outcomes(node = "Y")
#'
#' make_model("X->M->Y") |>
#' realise_outcomes(dos = list(M = 1), node = "Y")
#'}

realise_outcomes <- function(model,
                             dos = NULL,
                             node = NULL,
                             add_rownames = TRUE){

  # check dos + node specification
  if((!is.null(node)) && (length(node) > 1)) {
    stop("Please specify only one node")
  }

  # case with trivial single node model
  if(length(model$nodes) == 1) {
    data_realizations <- get_causal_types(model)
    if (add_rownames) {
      rownames(data_realizations) <-
        gsub("[[:alpha:]]", "", rownames(data_realizations))
    }
    return(data_realizations)
  }

  # case with node specified
  if(!is.null(node)) {
    # generate data realizations
    parents <- get_parents(model)[[node]]

    if(any(!names(dos) %in% parents)) {
      conjugation <- ifelse (sum(!names(dos) %in% parents) > 1,
                             "are not a parent of",
                             "is not a parent of")
      subjects <- paste0(names(dos)[!names(dos) %in% parents], collapse = ", ")
      if(all(!names(dos) %in% parents)){
        stop(paste(subjects, conjugation, node))
      } else {
        warning(paste(subjects, conjugation, node))
      }
    }

    nodes <- c(parents, node)
    nodal_types <- get_nodal_types(model)[nodes]

    # treat parents as exogenous and add dos
    for(p in parents) {
      if(p %in% names(dos)) {
        nodal_types[[p]] <- as.character(dos[[p]])
      } else {
        nodal_types[[p]] <- c("0","1")
      }
    }

    data_realizations <- expand.grid(nodal_types, stringsAsFactors = FALSE)
    colnames <- nodes
    names(data_realizations) <- nodes

    # rownames
    if(add_rownames) {
      types <- data_realizations
    }

    # generate parents_list
    parents_list <- as.list(rep(-1,length(parents)))
    parents_list <- c(parents_list, list((seq_along(parents)) - 1))
    names(parents_list) <- nodes

    # get variables to work through
    endogenous_vars <- length(nodes) - 1

    # turn data_realizations df into list to pass as
    # std::vector<std::vector<std::string>>
    n_types <- nrow(data_realizations)
    data_realizations <- as.list(data_realizations)
  }


  # case without node specified
  if(is.null(node)) {
    # get data realizations
    data_realizations <- get_causal_types(model)
    # replace parent names with parent col positions in data_realizations df
    # allows for faster access when df is converted to
    # std::vector<std::vector<std::string>> in c++
    parents_list  <- parents_to_int(get_parents(model), model$nodes)

    # rownames
    if(add_rownames) {
      types <- data_realizations
    }

    # put dos into data_realizations df
    if(!is.null(dos)) {
      data_realizations[,names(dos)] <- lapply(dos,as.character)
    }
    colnames <- model$nodes

    # get variables to work through
    endogenous_vars <- attr(model, "nonroot_nodes")
    endogenous_vars <- endogenous_vars[!endogenous_vars %in% names(dos)]
    endogenous_vars <-
      unname(vapply(endogenous_vars, function(i)
        which(i == model$nodes), numeric(1))) - 1

    # turn data_realizations df into list to pass as
    # std::vector<std::vector<std::string>>
    n_types <- nrow(data_realizations)
    data_realizations <- as.list(data_realizations)
  }



  data_realizations <- realise_outcomes_c(real = data_realizations,
                                          parents_list = parents_list,
                                          endogenous_vars = endogenous_vars,
                                          n_types = n_types) |>
    as.data.frame()

  colnames(data_realizations) <- colnames

  # add rownames
  if(add_rownames){
    rownames(data_realizations) <- apply(types, 1, paste, collapse = ".")
    type_names <-
      matrix(vapply(seq_along(types), function(j) {
        paste0(names(types)[j], types[, j])
      }, character(nrow(types))),
      ncol = ncol(types))
    attr(data_realizations, "type_names") <-
      apply(type_names, 1, paste,  collapse = ".")
  }
  return(data_realizations)
}


#' Helper to turn parents_list into a list of data_realizations column positions
#' @param parents_list a named list of character vectors specifying all
#'   nodes in the DAG and their respective parents
#' @return a list of column positions
#' @keywords internal
#' Used in realise_outcomes

parents_to_int <- function(parents_list, position_set) {
  out <- parents_list |>
    lapply(function(i) {
      if (length(i) > 0) {
        vapply(i, function(j) {
          which(j == position_set)
        }, numeric(1)) |>
          unname() - 1
      } else {
        -1
      }
    })

  return(out)
}


#' Reveal outcomes
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' This function was deprecated because the name causes clashes with
#' DeclareDesign. Use realise_outcomes instead.
#' @keywords internal
#'
reveal_outcomes <- function(model, dos = NULL, node = NULL) {
  lifecycle::deprecate_warn(
  "0.0.3.2",
  "reveal_outcomes()",
  details = paste("This function was deprecated because the name causes",
                  "clashes with DeclareDesign. Use realise_outcomes instead.")
)
  warning(paste("This function was deprecated because the name causes clashes",
                "with DeclareDesign. Use realise_outcomes instead."))
  realise_outcomes(model = model, dos = dos, node = node)
}




