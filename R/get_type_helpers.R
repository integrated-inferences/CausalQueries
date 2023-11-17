#' Identify nodes in a statement
#'
#'@param nodes A vector of characters. It should contain quoted names of
#'  the nodes in \code{model}
#'@param statement A character. A quoted causal statement.
#'@return Returns name of nodes present in a statement
#'@keywords internal
#'@importFrom stringr boundary str_split

nodes_in_statement <- function(nodes, statement) {
    nodes[nodes %in% str_split(statement, boundary("word"))[[1]]]
}

#' Returns a list with the nodes that are not directly pointing into a node
#'
#'@inheritParams CausalQueries_internal_inherit_params
#'@return Returns a list with the nodes that are not directly pointing into a node
#'@keywords internal

list_non_parents <- function(model, node) {
    node_parents <- get_parents(model)[[node]]
    not_parents <- !model$nodes %in% c(node_parents, node)
    model$nodes[not_parents]
}
#' Adds a wildcard for every missing parent
#'@inheritParams CausalQueries_internal_inherit_params
#'@param parents A vector of characters. The \code{node}'s parents
#'@param missing_parents A vector of characters.  The \code{node}'s missing parents
#'@return A causal query expression with all parents nodes set to either 0, 1 or wildcard '.'
#'@keywords internal

add_wildcard <- function(node,
                         statement,
                         parents,
                         missing_parents) {
    if (all(parents %in% missing_parents)) {
        q <- paste0(node, "[", paste0(missing_parents, " = . ", collapse = ", "), "]")
    } else if (length(missing_parents) > 0) {
        q <- gsub("\\]", paste0(", ", paste0(missing_parents, " = . ", collapse = ", "), "\\]"), statement)
    }
    return(q)
}
