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

	if(!is.null(model$P)) if(!is.null(model$confounds_df)) {
		conf_df <- get_confounds_df(model)
		inner <- paste(inner, " ; ", paste(paste(conf_df[,1], conf_df[,2], sep =  " <-> "), collapse = " ; "))
		}

	paste0("dag{ ",inner, " }")
}

#' Plot your dag using dagitty
#'
#' @param model A model created by make_model()
#' @param include_confounds Logical. Adds confounds by default. If model does not contain a confounds_df this is generated on the fly.
#'
#' @export
#'
#' @return dagitty translation of DAG
#' @importFrom graphics plot
#' @examples
#' \dontrun{
#' model <- make_model("X -> K -> Y; X -> Y")
#' plot(model)
#'
#' model <- make_model("X -> K -> Y; X <-> Y")
#' plot(model, include_confounds = FALSE)
#' plot(model)
#'
#' model <- make_model("X -> K -> Y; X <-> K; K <-> Y")
#' plot(model, include_confounds = FALSE)
#' plot(model)

#' }
#'

plot_dag <- function(model, include_confounds = TRUE){

	if(include_confounds & (!is.null(model$P)) & is.null(model$confounds_df)){
		message("Generating confounds_df on the fly")
		model <- set_confounds_df(model)}

	dagitty_dag <- translate_dagitty(model)
	plot(dagitty::graphLayout(dagitty_dag))
}


#' @export
plot.causal_model <- function(x, ...) {
		plot_dag(x, ...)
	}

