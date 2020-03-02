#' Puts your DAG into daggity syntax (useful for using their plotting functions)
#'
#' If confounds are indicated (provided in \code{attr(model$P, 'confounds')}), then these are represented as bidirectional arcs.
#'
#' @inheritParams gbiqq_internal_inherit_params
#'
#' @export
#'
#' @return dagitty translation of DAG
#'
#' @examples
#' \dontrun{
#' model <- make_model('X -> Y')
#' translate_dagitty(model)
#' }
#'
translate_dagitty <- function(model) {


    if (length(model$nodes) == 1)
        return(paste0("dag{ ", model$statement, " }"))

    dag <- model$dag
    children <- dag$children
    parents  <- dag$parent

    inner <- paste(paste0(apply(dag, 1, paste, collapse = " -> "), collapse = " ; "), collapse = "")

    if (!is.null(model$P) && !is.null(model$confounds_df) && all(!is.na(model$confounds_df))) {
        conf_df <- model$confounds_df
        inner <- paste(inner, " ; ", paste(paste(conf_df[, 1], conf_df[, 2], sep = " <-> "), collapse = " ; "))
    }

    if(any(children  == "")){
        isolates <- parents[children == ""]
        dag      <- filter(dag, children != "")
        inner    <- paste(inner, " ; ", paste0(isolates, collapse = " ; "))
    }

    dagitty_dag <- paste0("dag{ ", inner, " }")
    return(dagitty_dag)
}

#' Plot your dag using dagitty
#'
#'@inheritParams gbiqq_internal_inherit_params
#'
#' @export
#'
#' @return dagitty translation of DAG
#' @importFrom graphics plot
#' @examples
#' \dontrun{
#' model <- make_model('X -> K -> Y; X -> Y')
#' plot_dag(model)
#' model <- make_model('X -> K -> Y; X <-> Y')
#' plot_dag(model)
#' }
#'

plot_dag <- function(model) {
    if (!is.null(model$P) & is.null(model$confounds_df)) {
        message("Model has a P matrix but no confounds_df. confounds_df generated on the fly. To avoid this message try model <- set_confounds_df(model)")
        model <- set_confounds_df(model)
    }
    dagitty_dag <- translate_dagitty(model)
    plot(dagitty::graphLayout(dagitty_dag))
}

#' @export
plot.causal_model <- function(x, ...) {
    plot_dag(x)
}

