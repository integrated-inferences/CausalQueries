#' Plots a DAG in ggplot style using a causal model input
#'
#' If confounds are indicated (provided in \code{attr(model$P, 'confounds')}), then these are represented as bidirectional arcs.
#'
#' @param model A \code{causal_model} object generated from \code{make_model}
#' @param x_coord A vector of x coordinates for DAG nodes. If left empty, coordinates are randomly generated
#' @param y_coord A vector of y coordinates for DAG nodes. If left empty, coordinates are randomly generated
#' @param title String specifying title of graph
#' @param textcol String specifying color of text labels
#' @param textsize Numeric, size of text labels
#' @param shape Indicates shape of node. Defaults to circular node.
#' @param nodelcol String indicating color of node that is accepted by ggplot's default palette
#' @param nodesize Size of node.
#' @return A DAG plot in ggplot style.
#'
#' @keywords internal
#' @import ggdag
#' @import dplyr
#' @import ggplot2
#' @importFrom graphics plot
#' @importFrom dagitty dagitty
#' @importFrom repr repr_text
#' @importFrom stringr str_split
#' @importFrom latex2exp TeX
#'
#' @examples
#'
#' \dontrun{
#' make_model('X -> K -> Y; X <-> Y') %>%
#'   CausalQueries:::plot_dag()
#' make_model('X -> K -> Y') %>%
#'   set_confound('X <-> Y') %>%
#'   CausalQueries:::plot_dag()
#' CausalQueries:::plot_dag(model, x_coord = 1:3, y_coord = 1:3)
#' }
#'

plot_dag <- function(model = NULL,
                     x_coord = NULL,
                     y_coord = NULL,
                     title = "",
                     textcol = 'white',
                     textsize = 3.88,
                     shape = 16,
                     nodecol = 'black',
                     nodesize = 16
) {

    name <- to <- x <- y <- xend <- yend <- NULL
    # Checks
    if(is.null(model))
        stop("Model object must be provided")
    if(!is(model, "causal_model")) stop("Model object must be of type causal_model")
    if (is.null(x_coord) == !is.null(y_coord))
      message("Coordinates should be provided for both x and y (or neither).")
    if (!is.null(x_coord) & !is.null(y_coord) & length(x_coord)!=length(y_coord)) stop("x and y coordinates must be of equal length")
    if (!is.null(x_coord) & !is.null(y_coord) & length(model$nodes)!=length(x_coord)) stop("length of coordinates supplied must equal number of nodes")

    # Get names
    nodes <- model$nodes

    # Get statement

    dagitty_statement <-  paste("dag{", model$statement, "}") %>%
      dagitty::dagitty()


    # Add coordinates if provided (otherwise generated)

    if(!is.null(x_coord) & !is.null(y_coord)){
        names(x_coord) <- nodes
        names(y_coord) <- nodes

        dagitty::coordinates(dagitty_statement) <-
            list(x = x_coord , y = y_coord) %>%
            coords2df() %>% coords2list()
    }

    # Make the df
    df <- dagitty_statement %>% ggdag::tidy_dagitty()
    df$data <- df$data %>% mutate(
        update=paste0(name,to))


    # Step 2: Format and export
    df %>%
        ggplot(aes(x=x,y=y,xend=xend,yend=yend)) +
        geom_dag_point(colour=nodecol,shape=shape,
                       size=nodesize) +theme_dag() +
        labs(title = latex2exp::TeX(repr::repr_text(title) %>% stringr::str_remove_all('\\"'))) +
        geom_dag_text(aes_string(label = 'name'),
                      show.legend = FALSE,
                      parse = TRUE,
                      color=textcol,
                      size=textsize,
        ) +
        geom_dag_edges()
}

#' @export
plot.causal_model <- function(x, ...) {
    plot_dag(x,...)
}

