#' Identify nodes in statement
#'@param nodes A vector with quoted names of the nodes
#'@param statement A quoted causal statement.
#'@keywords internal
#' @importFrom stringr boundary str_split
nodes_in_statement <- function(nodes, statement)
	nodes[nodes %in% str_split(statement, boundary("word"))[[1]]]

#' Returns a list with the nodes that are not directly pointing into a node
#'@param node A quoted name of a node
#'@param statement A quoted causal statement.
#'@keywords internal
list_non_parents <- function(model, node){
	node_parents    <- get_parents(model)[[node]]
	not_parents     <- !model$nodes %in% c(node_parents, node)
   model$nodes[not_parents]
}
#'#' Adds a wildcard for every missing parent
#'@param node A quoted name of a node
#'@param statement A quoted causal statement.
#'@param parents node's parents
#'@param missing_parents node's missing parents
#'@keywords internal
add_wildcard <- function(node, statement, parents, missing_parents){
 if(all(parents %in% missing_parents)) {
 	q <-
 		paste0(node,"\\[", paste0(missing_parents, "= .", collapse = ", "), "\\]")
} else if(length(missing_parents) >0){
	q <-
		gsub("\\]", paste0(", ", paste0(missing_parents, "= .", collapse = ", "),"\\]"), statement)
}
	return(q)
}
