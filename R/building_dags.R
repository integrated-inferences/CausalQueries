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


#' Make a DAG
#'
#' @param ... Edges added by add_edges()
#'
#' @export
#'
#' @return A DAG
#'
make_dag <- function(...){
	dag <- rbind(...)
	intermediary <- (dag$children %in% dag$parent)
	pcm <- list(dag = rbind(dag[intermediary,], dag[!intermediary,]),
							step = "dag" )
  pcm <-
	return(pcm)
}

