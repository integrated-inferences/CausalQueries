#' Add edges to a DAG
#'
#' @param parent Character. The name of the parent variable.
#' @param chilren Character scalar or vector. The name(s) of the child(ren) variable(s).
#'
#' @export
#'
#' @return Edge(s) in a DAG
#'
#' @examples
#'
#'dag1 <- make_dag(
#' add_edges(parent = "X",children = c("K","Y")),
#' add_edges(parent = "K",children = "Y")
#' )
#'
add_edges <- function(parent,children){
	data.frame(parent = parent, children = children)
}


#' Make a DAG
#'
#' @param ... Edges added by add_edges()
#'
#' @export
#'
#' @return A DAG
#'
#' @examples
#'
#'
#'
make_dag <- function(...){
	rbind(...)
}

