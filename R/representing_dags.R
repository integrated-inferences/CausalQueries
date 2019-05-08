#' Puts your DAG into daggity syntax (useful for using their plotting functions)
#'
#' @param dag a dag from a model created by make_model()
#'
#' @export
#'
#' @return dagitty translation of DAG
#'
#' @examples
#' \dontrun{
#' model <- make_model(
#'  add_edges(parent = "X",children = c("K","Y")),
#'  add_edges(parent = "K",children = "Y")
#' )
#'
#' translate_dagitty(model$dag)
#' }
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
#' \dontrun{
#' model <- make_model(
#'  add_edges(parent = "X",children = c("K","Y")),
#'  add_edges(parent = "K",children = "Y")
#' )
#'
#' plot_dag(model)
#' }
#'

plot_dag <- function(model){
	dag <- model$dag
	dagitty_dag <- translate_dagitty(dag = dag)
	plot(dagitty::graphLayout(dagitty_dag))
}
