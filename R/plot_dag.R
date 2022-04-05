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
#' @import stringr
#' @importFrom graphics plot
#' @importFrom dagitty dagitty
#' @importFrom repr repr_text
#' @importFrom latex2exp TeX
#'
#' @examples
#'
#' \dontrun{
#' model <- CausalQueries:::make_model('X -> K -> Y; X <-> Y')
#' CausalQueries:::plot_dag(model)
#'
#' labels <- c(expression(paste(theta^A)),expression(paste(theta^B)),expression(paste(theta^C)))
#' CausalQueries:::plot_dag(model,x_coord=c(1,2,3),y_coord=c(3,2,1),lab_names=labels,labels = TRUE)
#' }
#'

plot_dag <- function(model = NULL,
                     x_coord = NULL,
                     y_coord = NULL,
                     lab_names = NULL,
                     labels = FALSE,
                     title = "",
                     textcol = 'white',
                     textsize = 3.88,
                     shape = 16,
                     nodecol = 'black',
                     nodesize = 16,
                     labsize = 3.88,
                     ...
) {
  # Checks
  if (is.null(model))
    stop("Model object must be provided")
  if (class(model)!="causal_model") stop("Model object must be of type causal_model")
  if (is.null(x_coord) & is.null(y_coord)) message("neither x nor y coordinates supplied; coordinates will be randomly generated")
  if (is.null(x_coord) & !is.null(y_coord)) stop("x coordinates are missing")
  if (!is.null(x_coord) & is.null(y_coord)) stop("y coordinates are missing")
  if (!is.null(x_coord) & !is.null(y_coord) & length(x_coord)!=length(y_coord)) stop("x and y coordinates must be of equal length")
  if (!is.null(x_coord) & !is.null(y_coord) & length(model$nodes)!=length(x_coord)) stop("length of coordinates supplied must equal number of nodes")
  if (!is.null(lab_names) & length(model$nodes)!=length(lab_names)) stop("length of labels supplied must equal number of nodes")
  if (!is.null(model$P) & is.null(model$confounds_df)) {
    message("Model has a P matrix but no confounds_df. confounds_df generated on the fly. To avoid this message try model <- set_confounds_df(model)")
    model <- set_confounds_df(model)
  }

  # Get names
  nodes <- if (is.null(lab_names)) model$nodes else LETTERS[1:length(lab_names)]

  statement <- ifelse(!is.null(lab_names),
                      stringr::str_replace_all(model$statement,setNames(nodes,model$nodes)),
                      model$statement)

  # Get statement

  dagitty_statement <-  paste("dag{", statement, "}") %>%
    dagitty::dagitty()


  # Add coordinates if provided (otherwise generated)

  if(!is.null(x_coord) & !is.null(y_coord)){
    names(x_coord) <- nodes
    names(y_coord) <- nodes

    dagitty::coordinates(dagitty_statement) <-
      list(x = x_coord , y = y_coord) %>%
      ggdag::coords2df() %>% ggdag::coords2list()
  }

  # Make the df
  df <- dagitty_statement %>% tidy_dagitty()
  df$data <- df$data %>% mutate(
    label = if(is.null(lab_names)) name else
      lab_names %>% as.character %>% .[match(df$data$name,LETTERS)],
    end = if(is.null(lab_names)) to else
      lab_names %>% as.character %>% .[match(df$data$to,LETTERS)],
    update=paste0(label,end),
    pos=match(label,lab_names)) %>%
    arrange(-desc(pos))


  # Step 2: Format and export
  p <- df %>%
    ggplot(aes(x=x,y=y,xend=xend,yend=yend)) +
    geom_dag_point(colour=nodecol,shape=shape,
                   size=nodesize) +theme_dag() +
    labs(title = TeX(repr_text(title) %>% stringr::str_remove_all('\\"')))

  if (labels==TRUE){
    p +
      geom_dag_label_repel(aes_string(label = 'label'),
                           show.legend = FALSE,
                           parse = TRUE,
                           size = labsize,
                           ...) +
      geom_dag_edges()

  } else {
    # Labels centered on nodes
    p +
      geom_dag_text_repel(aes_string(label = 'label'),
                          show.legend = FALSE,
                          parse = TRUE,
                          color=textcol,
                          size=textsize,
                          box.padding = 0,
                          force = 0
      ) + geom_dag_edges()
  }
}

#' @export
plot.causal_model <- function(x, ...) {
  plot_dag(x,...)
}

