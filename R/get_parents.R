

#' Get list of parents of all nodes in a model
#'
#' @inheritParams CausalQueries_internal_inherit_params
#' @return A \code{list} of parents in a DAG

get_parents <- function(model) {
  parents <- lapply(model$nodes, function(j) {
    paste(model$dag$parent)[paste(model$dag$children) == j]
  })
  names(parents) <- model$nodes
  class(parents) <- c("parents", "list")
  return(parents)
}

