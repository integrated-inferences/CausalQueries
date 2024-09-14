#' Get list of parents of all nodes in a model
#'
#' @inheritParams CausalQueries_internal_inherit_params
#' @return A \code{list} of parents in a DAG

get_parents <- function(model) {
  parents <- list()

  for (i in 1:nrow(model$parents_df)) {
    node <- model$parents_df$node[i]
    parent_nodes <- model$parents_df$parent_nodes[i]

    if (parent_nodes == "") {
      parents[[node]] <- character(0)  # Empty character vector for no parents
    } else {
      parents[[node]] <- strsplit(parent_nodes, ", ")[[1]]  # Split parent nodes by comma and space
    }
  }

  class(parents) <- "list"
  return(parents)

}
