#' Add edges to a DAG
#'
#' @param parent Character. The name of the parent variable.
#' @param children Character scalar or vector. The name(s) of the child(ren) variable(s).
#'
#' @export
#'
#' @return Edge(s) in a DAG
#'
#' @examples
#' \dontrun{
#'dag1 <- make_dag(
#' add_edges(parent = "X",children = c("K","Y")),
#' add_edges(parent = "K",children = "Y")
#' )
#' }
add_edges <- function(parent,children){
	data.frame(parent = parent, children = children)
}


#' Make a model
#'
#' `make_model` requires specifying nodes and edges of a graph. Imlpied causal types are calculated and default priors are provided under the assumption of no confounding.
#' Models can be updated with specification of a parameter matrix, P, by providing restrictions on causal types, and/or by providing informative priors on parameters. `
#'
#' @param ... Edges added by add_edges()
#'
#' @export
#'
#' @return An object of class probabilistic_causal_model containing a dag
#' @examples
#' \dontrun{
#' modelXKY <- make_model(
#' add_edges(parent = "X",children = c("K","Y")),
#' add_edges(parent = "K",children = "Y")
#' )
#' }
make_model <- function(...){
	dag <- rbind(...)
	intermediary <- (dag$children %in% dag$parent)
	dag <- list(dag = rbind(dag[intermediary,], dag[!intermediary,]),
							step = "dag" )

	model <- set_priors(dag)

	class(model) <- "probabilistic_causal_model"

	return(model)
}

