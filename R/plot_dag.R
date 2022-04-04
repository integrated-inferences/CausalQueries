#' Plots a DAG in ggplot style using a causal model input
#'
#' If confounds are indicated (provided in \code{attr(model$P, 'confounds')}), then these are represented as bidirectional arcs.
#'
#' @param model A \code{causal_model} object generated from \code{make_model}
#' @param x A vector of coordinates for DAG nodes. If left empty, coordinates are randomly generated
#' @param y A vector of coordinates for DAG nodes. If left empty, coordinates are randomly generated
#' @param title String specifying title of graph
#' @param textcol String specifying colour of text labels
#' @param textsize Numeric, size of text labels
#' @param obscure A vector or string of the form "X->Y" that indicates if a given arrow should be hidden
#' @param shape Indicates shape of node. Defaults to circular node.
#' @param nodelcol String indicating colour of node that is accepted by ggplot's default palette
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
#' model <- CausalQueries:::make_model('X -> K -> Y; X <-> Y')
#' CausalQueries:::plot_dag(model)
#' }
#'

plot_dag <- function(model = NULL,
                     x = NULL,
                     y = NULL,
                     title = "",
                     textcol = 'white',
                     textsize = 3.88,
                     obscure=NULL,
                     shape = 16,
                     nodecol = 'black',
                     nodesize = 16
) {
    # Checks
    if(is.null(model))
        stop("Model object must be provided")
    if(class(model)!="causal_model") stop("Model object must be of type causal_model")
    if (!is.null(x) & !is.null(y) & length(x)!=length(y)) stop("x and y coordinates must be of equal length")
    if (!is.null(x) & !is.null(y) & length(model$nodes)!=length(x)) stop("length of coordinates supplied must equal number of nodes")
    if (!is.null(model$P) & is.null(model$confounds_df)) {
        message("Model has a P matrix but no confounds_df. confounds_df generated on the fly. To avoid this message try model <- set_confounds_df(model)")
        model <- set_confounds_df(model)
    }

    # Get names
    nodes <- model$nodes

    # Get statement
    statement <- model$statement
    dagitty_statement <-  paste("dag{", statement, "}") %>% dagitty::dagitty()


    # Add coordinates if provided (otherwise generated)

    if(!is.null(x) & !is.null(y)){
        names(x) <- nodes
        names(y) <- nodes

        coordinates(dagitty_statement) <-
            list(x = x , y = y) %>%
            coords2df() %>% coords2list()
    }

    # Make the df
    df <- dagitty_statement %>% ggdag::tidy_dagitty()
    df$data <- df$data %>% mutate(
        update=paste0(name,to))

    #matching bit is necessary because the dataframe doesn't always list all names in the order you first specify

    # remove any arrows to be obscured
    if (!is.null(obscure)) {obscoords<-data.frame(update = lapply(obscure %>%
                                                                      str_split('->'),paste,collapse='') %>%
                                                      unlist())
    df$data$direction[match(obscoords$update,df$data$update)]<-NA}

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

