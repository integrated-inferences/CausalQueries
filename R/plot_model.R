#' Plots a DAG in ggplot style using a causal model input
#'
#' If confounds are indicated (provided in \code{attr(model$P, 'confounds')}),
#' then these are represented as bidirectional arcs. Builds on functionality
#' from ggdag and dagitty.
#'
#' @param model A \code{causal_model} object generated from \code{make_model} or a  \code{causal_statement} of the form "X->Y"
#' @param nodes model nodes, used when x_coord and y_coord are provided
#' @param x_coord A vector of x coordinates for model nodes.
#'   If left empty, coordinates are randomly generated
#' @param y_coord A vector of y coordinates for model nodes.
#'   If left empty, coordinates are randomly generated
#' @param title String specifying title of graph
#' @param textcol String specifying color of text labels
#' @param textsize Numeric, size of text labels
#' @param shape Indicates shape of node. Defaults to circular node.
#' @param nodecol String indicating color of node that is accepted by
#'   ggplot's default palette
#' @param nodesize Size of node.
#' @return A DAG plot in ggplot style.
#'
#' @keywords internal
#' @import ggdag
#' @import dplyr
#' @import ggplot2
#' @importFrom graphics plot
#' @importFrom dagitty dagitty
#' @importFrom stringr str_split
#' @importFrom latex2exp TeX
#'
#' @export
#' @examples
#'
#' \dontrun{
#' model <- make_model('X -> K -> Y; X <-> Y')
#'
#' model |>
#'   plot_model()
#'
#' model |>
#'   plot_model(
#'     x_coord = 1:3,
#'     y_coord = 1:3,
#'     title = "Mixed text and math: $\\alpha^2 + \\Gamma$")
#' }
#'
#' plot_model("X -> Y")
#'
#' plot_model("X -> Y", x_coord = 1:2, y_coord = 1:2, nodes = c("Y", "X"))
#'

plot_model <- function(model = NULL,
                       nodes = NULL,
                       x_coord = NULL,
                       y_coord = NULL,
                       title = "",
                       textcol = 'white',
                       textsize = 3.88,
                       shape = 16,
                       nodecol = 'black',
                       nodesize = 16) {
  # Checks

  if (!any(c("character", "causal_model") %in% class(model)))
    stop("First argument should be a causal model or a causal statement")


  if (is.null(x_coord) == !is.null(y_coord)) {
    message("Coordinates should be provided for both x and y (or neither).")
  }


  if (!is.null(x_coord) & !is.null(y_coord)) {
    if (length(x_coord) != length(y_coord))
      stop("x and y coordinates must be of equal length")

    if (is.null(nodes) & is(model, "causal_model"))
      nodes <- grab(model, "nodes")

    if (is.null(nodes) & !is.null(names("x_coord")))
      nodes <- names("x_coord")

    if (is.null(nodes))
      stop("nodes should be provided when x_coord and y_coord are supplied")

    if (length(nodes) != length(x_coord))
      stop("length of coordinates supplied must equal number of nodes")
  }

  # Get statement

  statement <- ifelse(is(model, "model"), grab(model, "causal_statement"), model)

  # pars

  name <- to <- x <- y <- xend <- yend <- NULL


  dagitty_statement <-  paste("dag{", statement, "}") |>
    dagitty::dagitty()

  # Add coordinates if provided (otherwise generated)
  if (!is.null(x_coord) & !is.null(y_coord)) {
    names(x_coord) <- names(y_coord) <- nodes

    dagitty::coordinates(dagitty_statement) <-
      list(x = x_coord , y = y_coord) |>
      coords2df() |>
      coords2list()
  }

  # Make the df
  df <- dagitty_statement |>
    ggdag::tidy_dagitty()

  df$data <- df$data |>
    dplyr::mutate(update = paste0(name, to))

  aes_list <- lapply(list(label = 'name'), function(i)
    dplyr::sym(i))

  # Step 2: Format and export
  df |>
    ggplot(aes(
      x = x,
      y = y,
      xend = xend,
      yend = yend
    )) +
    geom_dag_point(colour = nodecol,
                   shape = shape,
                   size = nodesize) +
    theme_dag() +
    labs(title = latex2exp::TeX(title)) +
    geom_dag_text(
      aes(!!!aes_list),
      show.legend = FALSE,
      parse = TRUE,
      color = textcol,
      size = textsize,
    ) +
    geom_dag_edges()
}

#' @export
plot.causal_model <- function(x, ...) {
  plot_model(x, ...)
}

# ' Alias
plot_dag <- plot_model
