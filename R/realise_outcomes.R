#' Realise outcomes
#'
#' Realise outcomes for all causal types. Calculated by sequentially calculating endogenous nodes.
#' If a do operator is applied to any node then it takes the given value and all its descendants are generated accordingly.
#'
#' @details \code{realise_outcomes} starts off by creating types (via \code{\link{get_nodal_types}}). It then takes types of endogenous and reveals their outcome based on the value that their parents took. Exogenous nodes outcomes correspond to their type.
#' @inheritParams CausalQueries_internal_inherit_params
#' @param dos A named \code{list}. Do actions defining node values, e.g., \code{list(X = 0, M = 1)}.
#' @param node A character. An optional quoted name of the node whose outcome should be revealed. If specified all values of parents need to be specified via \code{dos}.
#' @param add_rownames logical indicating whether to add causal types as rownames to the output
#' @return A \code{data.frame} object of revealed data for each node (columns) given causal / nodal type (rows) .
#' @export
#' @examples
#' \donttest{
#' make_model("X -> Y") |>
#'   realise_outcomes()
#'
#'make_model("X -> Y <- W") |>
#' set_restrictions(labels = list(X = "1", Y="0010"), keep = TRUE) |>
#'  realise_outcomes()
#'
#' make_model("X1->Y; X2->M; M->Y") |>
#' realise_outcomes(dos = list(X1 = 1, M = 0))
#'
#' make_model("X->M->Y") |>
#' realise_outcomes(dos = list(M = 1), node = "Y")
#'}

realise_outcomes <- function(model, dos = NULL, node = NULL, add_rownames = TRUE){

  # check dos + node specification
  if(!is.null(node)) {

    if(length(node) > 1) {
      stop("Please specify only one node")
    }

    if(is.null(dos)) {
      stop("Do actions must be specified when node is not NULL")
    }

  }

  # get causal types
  data_realizations <- get_causal_types(model)

  # case with trivial single node model
  if(length(model$nodes) == 1) {
    if (add_rownames) {
      rownames(data_realizations) <-
        gsub("[[:alpha:]]", "", rownames(data_realizations))
    }
    return(data_realizations)
  }


  # case with node specified
  if(!is.null(node)) {
    # generate data realizations
    parents <- get_parents(model)
    sub_dag <- traverse_dag(node = node, dag = parents, dos = names(dos))

    if(any(!names(dos) %in% sub_dag)) {
      conjugation <- ifelse (sum(!names(dos) %in% sub_dag)>1, "are not a cause of", "is not a cause of")
      subjects <- paste0(names(dos)[!names(dos) %in% sub_dag], collapse = ", ")
      if(all(!names(dos) %in% sub_dag)){
        stop(paste(subjects, conjugation, node))
      } else {
        warning(paste(subjects, conjugation, node))
      }
    }

    nodal_types <- get_nodal_types(model)
    nodal_types <- nodal_types[sort(match(sub_dag, names(nodal_types)))]
    data_realizations <- expand.grid(nodal_types, stringsAsFactors = FALSE)

    nodes <- names(nodal_types)
    colnames <- nodes
    names(data_realizations) <- nodes

    # rownames
    if(add_rownames) {
      types <- data_realizations
    }

    # put dos into data_realizations df
    if(!is.null(dos)) {
      data_realizations[,names(dos)] <- lapply(dos,as.character)
    }

    # generate parents_list
    parents_list <- parents[nodes]

    for(i in 1:length(dos)) {
      parents_list[[names(dos)[i]]] <- character(0)
    }

    # get variables to work through
    endogenous_vars <- names(parents_list)[sapply(parents_list, length) != 0]
    endogenous_vars <- which(endogenous_vars == nodes) - 1

    # replace parent names with parent col positions in data_realizations df
    # allows for faster access when df is converted to std::vector<std::vector<std::string>> in c++
    parents_list <- parents_to_int(parents_list, nodes)


    # turn data_realizations df into list to pass as std::vector<std::vector<std::string>>
    n_types <- nrow(data_realizations)
    data_realizations <- as.list(data_realizations)
  }


  # case without node specified
  if(is.null(node)) {
    # replace parent names with parent col positions in data_realizations df
    # allows for faster access when df is converted to std::vector<std::vector<std::string>> in c++
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
    endogenous_vars <- unname(sapply(endogenous_vars, function(i) which(i == model$nodes))) - 1

    # turn data_realizations df into list to pass as std::vector<std::vector<std::string>>
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
    type_names <- matrix(sapply(1:ncol(types), function(j) paste0(names(types)[j], types[,j])), ncol = ncol(types))
    attr(data_realizations, "type_names") <- apply(type_names, 1, paste,  collapse = ".")
  }

  return(data_realizations)
}


#' Helper to generate a sub-DAG by recursively traversing a DAG from a given node until a root node or a node with a do-operation is reached
#' @param node a string specifying a node in the DAG
#' @param dag a named list of character vectors specifying all nodes in the DAG and their respective parents
#' @return a character vector of nodes
#' @keywords internal
#' Used in realise_outcomes

traverse_dag <- function(node, dag, dos = character(0)) {
  if (node %in% names(dag)) {
    if (node %in% dos) {
      return(node)
    } else {
      parents <- dag[[node]]
      if (length(parents) == 0) {
        return(node)
      } else {
        parent_list <- c(node)
        for (parent in parents) {
          parent_list <- c(parent_list, traverse_dag(parent, dag, dos))
        }
        return(unique(parent_list))
      }
    }
  } else {
    stop("Node not found in the DAG")
  }
}

#' Helper to turn parents_list into a list of data_realizations column positions
#' @param parents_list a named list of character vectors specifying all nodes in the DAG and their respective parents
#' @return a list of column positions
#' @keywords internal
#' Used in realise_outcomes

parents_to_int <- function(parents_list, position_set) {
  out <- parents_list |>
    lapply(function(i) {
      if (length(i) > 0) {
        sapply(i, function(j)
          which(j == position_set)) |>
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
#' This function was deprecated because the name causes clashes with DeclareDesign. Use realise_outcomes instead.
#' @keywords internal
#'
reveal_outcomes <- function(model, dos = NULL, node = NULL) {
  lifecycle::deprecate_warn(
  "0.0.3.2",
  "reveal_outcomes()",
  details = "This function was deprecated because the name causes clashes with DeclareDesign. Use realise_outcomes instead."
)
  warning("This function was deprecated because the name causes clashes with DeclareDesign. Use realise_outcomes instead.")
  realise_outcomes(model = model, dos = dos, node = node)
}




