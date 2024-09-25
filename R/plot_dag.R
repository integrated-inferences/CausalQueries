#' Plots a DAG in ggplot style using a causal model input
#'
#' Creates a plot of a DAG using ggplot functionality and a Sugiyama layout from igraph.  Unmeasured confounds  (<->) are indicated
#' then these are represented as curved dotted lines.  Users can control node sizes and colors as well as coordinates and label behavior. Other modifications can be made by adding additional ggplot layers.
#'
#' @param model A \code{causal_model} object generated from \code{make_model}
#' @param x_coord A vector of x coordinates for DAG nodes.
#'   If left empty, coordinates are randomly generated
#' @param y_coord A vector of y coordinates for DAG nodes.
#'   If left empty, coordinates are randomly generated
#' @param labels Optional labels for nodes
#' @param title String specifying title of graph
#' @param textcol String specifying color of text labels
#' @param textsize Numeric, size of text labels
#' @param shape Indicates shape of node. Defaults to circular node.
#' @param nodecol String indicating color of node that is accepted by
#'   ggplot's default palette
#' @param nodesize Size of node.
#' @param layout see
#' @return A ggplot object.
#'
#' @keywords internal
#' @import dplyr
#' @import ggplot2
#' @import ggraph
#' @importFrom grid arrow
#' @importFrom grid unit
#' @importFrom graphics plot
#' @importFrom latex2exp TeX
#'
#' @export
#' @examples
#'
#' \dontrun{
#' model <- make_model('X -> K -> Y')
#'
#' # Simple plot
#' model |> plot_model()
#'
#' # Adding additional layers
#' model |> plot_model() +
#'   ggplot2:: theme(panel.border = ggplot2::element_rect(fill=NA))
#'
#' # Adding labels
#' model |>
#'   plot_model(
#'     labels = c("A long \n one", "This", "That"),
#'     nodecol = "white", textcol = "black")
#'
#' # Controlling  positions and using math labels
#' model |> plot_model(
#'     x_coord = 0:2,
#'     y_coord = 0:2,
#'     title = "Mixed text and math: $\\alpha^2 + \\Gamma$")
#' }
#'
#' # DAG with unobserved confounding
#' make_model('X -> K -> Y; X <-> Y') |> plot()
#'


plot_model <- function(model = NULL,
                       x_coord = NULL,
                       y_coord = NULL,
                       labels = NULL,
                       title = "",
                       textcol = 'white',
                       textsize = 3.88,
                       shape = 16,
                       nodecol = 'black',
                       nodesize = 16
) {

  # Checks
  if(is.null(model)) {
    stop("Model object must be provided")
  }

  if(!is(model, "causal_model")) {
    stop("Model object must be of type causal_model")
  }

  if (is.null(x_coord) == !is.null(y_coord)) {
    message("Coordinates should be provided for both x and y (or neither).")
    x_coord <- NULL
    y_coord <- NULL

  }

  if (!is.null(x_coord) &
      !is.null(y_coord) &
      length(x_coord) != length(y_coord)) {
    stop("x and y coordinates must be of equal length")
  }

  if (!is.null(x_coord) &
      !is.null(y_coord) &
      length(model$nodes) != length(x_coord)) {
    stop("length of coordinates supplied must equal number of nodes")
  }

  if (!is.null(labels) &
      (length(model$nodes) != length(labels))) {
    stop("length of labels supplied must equal number of nodes")
  }

  # generate dag frame
  dag <-
    model$statement |>
    make_dag() |>
    dplyr::rename(x = v, y = w) |>
    dplyr::mutate(weight = 1)

  # Figure our ggraph data structure
  coords <- (dag  |> ggraph::ggraph(layout = "sugiyama"))$data |>
    dplyr::select(x, y, name)

  # Manual coordinate  override
  nodes <- as.character(model$nodes)
  if (!is.null(x_coord)){
    coords$x <- x_coord[match(coords$name, nodes)]
    coords$y <- y_coord[match(coords$name, nodes)]
  }

  # Manual labels override
  if (!is.null(labels))
    coords$name <- labels[match(coords$name, nodes)]

  # plot
   dag  |>
      ggraph::ggraph(layout = "manual", x = coords$x, y = coords$y) +
      ggraph::geom_edge_arc(data = arc_selector("<->"),
                  start_cap = ggraph::circle(8, 'mm'),
                  end_cap = ggraph::circle(8, 'mm'),
                  linetype = "dashed") +
      ggraph::geom_edge_link(data = arc_selector("->"),
                   arrow = grid::arrow(length = grid::unit(4, 'mm'), type = "closed"),
                   start_cap = ggraph::circle(8, 'mm'),
                   end_cap = ggraph::circle(8, 'mm'))  +
      geom_point(data = coords,
                 aes(x, y),
                 size = nodesize,
                 color = nodecol,
                 shape = shape) +
      theme_void()  +
      geom_node_text(
        data = coords,
        aes(x, y, label = name),
        color = textcol,
        size = textsize) +
      ggplot2::labs(title = latex2exp::TeX(title))

}

#' @export
plot.causal_model <- function(x, ...) {
  plot_model(x,...)
}


#' Alias
plot_dag <- plot_model


## helper

arc_selector <- function(x) {
  function(layout) {
    edges <- get_edges()(layout)
    res <- edges[edges$e == x, ]
    res
  }
}





