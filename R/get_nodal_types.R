#' Get list of types for nodes in a DAG
#'
#' As type labels are hard to interpret for large models, the type list
#' includes an attribute to help interpret them.
#' See  \code{attr(types, interpret)}
#'
#' @inheritParams CausalQueries_internal_inherit_params
#' @param collapse Logical. If `TRUE`, shows unique nodal types for each node.
#'   If `FALSE`, shows for each node a matrix with nodal types as rows and
#'   parent types as columns, if applicable. Defaults to `TRUE`.
#' @keywords internal
#' @importFrom rlang is_empty
#' @return A named \code{list} of nodal types for each parent in a DAG

get_nodal_types <- function(model, collapse = TRUE) {
  # 1 Extract nodal types if these exist (and collapsed format sought)
  if (collapse & !is.null(model$nodal_types)) {
    return(model$nodal_types)
  }

  # 1b Extract nodal types if these exist
  # (and uncollapse if uncollapse sought)
  if (!collapse & !is.null(model$nodal_types)) {
    return(uncollapse_nodal_types(model$nodal_types))
  }

  # 2. Create and interpret list of nodal types
  nodal_types <- make_nodal_types(model)

  # reduce if necessary
  if (!is.null(model$nodal_types)) {
    nodal_types <- lapply(model$nodes, function(v) {
      mat <- nodal_types[[v]]
      mat[model$nodal_types[[v]],]
    })
    names(nodal_types) <- model$nodes
  }

  # 3. Optionally provide in collapsed form
  if (collapse) {
    nodal_types <- collapse_nodal_types(nodal_types)
  }

  attr(nodal_types, "interpret") <- interpret_type(model)
  class(nodal_types) <- c("nodal_types", "list")

  return(nodal_types)
}

#' uncollapse nodal types
#'
#' @param nodal_types A list of nodal types in collapsed form ('01', '11') etc..
#' @return A \code{list} containing nodes with nodal types in data.frame form
#' @keywords internal

uncollapse_nodal_types <- function(nodal_types) {
  x <- nodal_types |>
    lapply(stringr::str_split, "")  |>
    lapply(data.frame) |>
    lapply(t) |>
    lapply(function(df)
      apply(df, 2, as.numeric)) |>
    lapply(data.frame)

  # To handle cases where a single nodal type exists
  # otherwise it gets wrongly transposed
  for (j in seq_along(x)) {
    if (length(nodal_types[[j]]) == 1) {
      x[[j]] <- t(x[[j]])
    }
  }

  for (j in seq_along(x)) {
    # Add row names
    rownames(x[[j]]) <- apply(x[[j]], 1, paste, collapse = "")
    # Add col names
    colnames(x[[j]]) <- perm(rep(1, log(ncol(x[[j]]), 2))) %>%
      apply(1, paste, collapse = "")
  }

  class(x) <- c("nodal_types", "list")

  return(x)
}


#' Make nodal types
#'
#' @inheritParams CausalQueries_internal_inherit_params
#' @param include_node_names Logical. If `TRUE` returns names of form X0, X1;
#'   otherwise returns 0, 1. Defaults to `FALSE`
#' @return A named list containing nodal types for each node
#' @keywords internal
#' @examples
#' \donttest{
#' model <- make_model('X -> K -> Y')
#' CausalQueries:::make_nodal_types(model)
#' }
make_nodal_types <- function(model,
                             include_node_names = FALSE) {
  nodes <- model$nodes
  real_node <- !(is.na(nodes))
  nodes <- nodes[real_node]
  parents <- get_parents(model)[real_node]
  nodal_types <- lapply(lapply(parents, length), type_matrix)

  nodal_types_labels <-
    lapply(seq_along(nodal_types), function(i) {
      labels <- apply(nodal_types[[i]], 1, paste, collapse = "")
      if (include_node_names) {
        return(paste0(names(nodal_types)[i], labels))
      }
      labels
    })

  names(nodal_types_labels) <- nodes

  # Add row labels
  nodal_types <- lapply(nodes, function(v) {
    rownames(nodal_types[[v]]) <- nodal_types_labels[[v]]
    nodal_types[[v]]
  })

  names(nodal_types) <- nodes
  class(nodal_types) <- c("nodal_types", "list")

 return(nodal_types)
}

#' collapse nodal types
#' @param nodal_types A list of nodal types.
#' @param include_node_names Logical, if TRUE returns names X0, X1;
#'   otherwise returns 0, 1
#' @return A \code{list} containing nodes with nodal types in a vector form.
#' @keywords internal
#' @examples
#'
#' model <- make_model('X -> K -> Y')
#' (nodal_types <- grab(model, "nodal_types", collapse = FALSE))
#' CausalQueries:::collapse_nodal_types(nodal_types )
collapse_nodal_types <- function(nodal_types,
                                 include_node_names = FALSE) {
    # Skip if already collapsed
    if (!(is.data.frame(nodal_types[[1]]))) {
      return(nodal_types)
    }

    # Otherwise collapse
    types <- lapply(seq_along(nodal_types), function(i) {
      var <- names(nodal_types)[i]
      mat <- as.matrix(nodal_types[[i]])
      labels <- apply(mat, 1, paste, collapse = "")
      if (include_node_names) {
        return(paste0(var, labels))
      }
      paste0(labels)
    })
    names(types) <- names(nodal_types)
    class(types) <- c("nodal_types", "list")

    return(types)
  }


#' Generate type matrix
#' @param parent_n An integer. Number of parents of a given child.
#' @return A \code{data.frame} whose rows contain digits of
#'   each causal types in a model
#' @keywords internal
#' @examples
#' \donttest{
#' CausalQueries:::type_matrix(2)
#' }
type_matrix <- function(parent_n) {
  type_mat <- perm(rep(1, 2 ^ parent_n))
  if (parent_n == 0) {
    labels <- NULL
  } else {
    input_mat <- perm(rep(1, parent_n))
    labels <- apply(input_mat, 1, paste, collapse = "")
  }
  colnames(type_mat) <- labels
  return(type_mat)
}
