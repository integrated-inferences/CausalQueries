#' Puts your DAG into daggity syntax (useful for using their plotting functions)
#'
#' @param dag A dag created by make_dag()
#'
#' @export
#'
#' @return dagitty translation of DAG
#'
#' @examples
#'
#' dag <- make_dag(
#'  add_edges(parent = "X",children = c("K","Y")),
#'  add_edges(parent = "K",children = "Y")
#' )
#'
#' translate_dagitty(dag)
#'
#'
translate_dagitty <- function(dag){
	inner <- paste(paste0(apply(dag,1,paste,collapse = " -> "),collapse = " ; "),collapse = "")
	dagitty_dag <- paste0("dag{ ",inner, " }")
	return(dagitty_dag)
}

#' Plot your dag using dagitty
#'
#' @param dag A dag created by make_dag()
#'
#' @export
#'
#' @return dagitty translation of DAG
#'
#' @examples
#'
#' dag <- make_dag(
#'  add_edges(parent = "X",children = c("K","Y")),
#'  add_edges(parent = "K",children = "Y")
#' )
#'
#' plot_dag(dag)
#'
#'

plot_dag <- function(dag){
	dagitty_dag <- translate_dagitty(dag = dag)
	plot(dagitty::graphLayout(dagitty_dag))
}
