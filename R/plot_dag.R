#' Puts your DAG into daggity syntax (useful for using their plotting functions)
#'
#' If confounds are indicated (provided in \code{attr(model$P, "confounds")}), then these are represented as bidirectional arcs.
#'
#' @param model A model created by make_model()
#'
#' @export
#'
#' @return dagitty translation of DAG
#'
#' @examples
#' \dontrun{
#' model <- make_model("X -> Y")
#' translate_dagitty(model)
#' }
#'
translate_dagitty <- function(model){
  dag   <- model$dag
	inner <- paste(paste0(apply(dag,1,paste,collapse = " -> "), collapse = " ; "),collapse = "")

	if(!is.null(model$P)) if(!is.null(attr(model$P, "confounds"))) {
		conf_df <- attr(model$P, "confounds")
		inner <- paste(inner, " ; ", paste(paste(conf_df[,1], conf_df[,2], sep =  " <-> "), collapse = " ; "))
		}

	dagitty_dag <- paste0("dag{ ",inner, " }")
	return(dagitty_dag)
}

#' Plot your dag using dagitty
#'
#' @param model A dag created by make_model()
#'
#' @export
#'
#' @return dagitty translation of DAG
#' @importFrom graphics plot
#' @examples
#' \dontrun{
#' model <- make_model("X -> K -> Y; X -> Y")
#' plot_dag(model)
#' }
#'

plot_dag <- function(model){
	dagitty_dag <- translate_dagitty(model)
	plot(dagitty::graphLayout(dagitty_dag))
}


#' @export
plot.causal_model <- function(x, ...) {
		plot_dag(x)
	}

