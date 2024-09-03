#' Plots a DAG in ggplot style using a causal model input
#'
#' If confounds are indicated (provided in \code{attr(model$P, 'confounds')}),
#' then these are represented as bidirectional arcs.
#'
#' @param model A \code{causal_model} object generated from \code{make_model}
#' @param x_coord A vector of x coordinates for DAG nodes.
#'   If left empty, coordinates are randomly generated
#' @param y_coord A vector of y coordinates for DAG nodes.
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
#' @import dplyr
#' @import ggplot2
#' @importFrom graphics plot
#' @importFrom latex2exp TeX
#'
#' @export
#' @examples
#'
#' \dontrun{
#' model <- make_model('X -> K -> Y; X <-> Y')
#'
#' model |>
#'   CausalQueries:::plot_model()
#' model |>
#'   CausalQueries:::plot_model(
#'     x_coord = 1:3,
#'     y_coord = 1:3,
#'     title = "Mixed text and math: $\\alpha^2 + \\Gamma$")
#' }
#'

plot_model <- function(model = NULL,
                       x_coord = NULL,
                       y_coord = NULL,
                       title = "",
                       textcol = 'white',
                       textsize = 3.88,
                       shape = 16,
                       nodecol = 'black',
                       nodesize = 16
) {

  x <- y <- xend <- yend <- name <- NULL

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

  # generate coordinates
  statement <- model$statement
  nodes <- as.character(model$nodes)

  dag <- make_dag(statement)

  if (is.null(x_coord) && is.null(y_coord)) {
    coords <- position_nodes(dag, nodes)
  } else {
    coords <- data.frame(
      node = nodes,
      x = x_coord,
      y = y_coord
    )
  }

  coords <- coords |>
    dplyr::mutate(x = x - mean(coords$x),
                  y = y - mean(coords$y))

  dag <- dag |>
    dplyr::left_join(coords, by = list(x = "w", y = "node")) |>
    magrittr::set_colnames(c("name","to","direction","xend","yend"))

  dag_plot <- data.frame(
    name = nodes
  ) |>
    dplyr::left_join(coords, by = list(x = "name", y = "node")) |>
    dplyr::left_join(dag, "name")

  # set plot extent
  extent_adjust <- nodesize / 100

  extent <- list(
    range(dag_plot$x, na.rm = TRUE) + c(-extent_adjust, extent_adjust),
    range(dag_plot$y, na.rm = TRUE) + c(-extent_adjust, extent_adjust)
  )

  # avoid edge / node overlap
  edges <- adjust_edge(dag_plot, nodesize)

  # plot
  ggplot() +
    geom_point(
      data = dplyr::distinct(dag_plot, name, .keep_all = TRUE),
      aes(x = x, y = y),
      size = nodesize, color = nodecol, shape = shape
    ) +
    geom_text(
      data = dplyr::distinct(dag_plot, name, .keep_all = TRUE),
      aes(x = x, y = y, label = name),
      size = textsize, color = textcol
    ) +
    geom_segment(
      data = edges[edges$direction == "->", ],
      aes(x = x, y = y, xend = xend, yend = yend),
      arrow = arrow(type = "closed", length = unit(5, "pt"))
    ) +
    geom_curve(
      data = edges[edges$direction == "<->", ],
      aes(x = x, y = y, xend = xend, yend = yend),
      curvature = 0.3,
      arrow = arrow(type = "closed", ends = "both", length = unit(5, "pt"))
    ) +
    labs(title = latex2exp::TeX(title)) +
    scale_x_continuous(limits = extent[[1]]) +
    scale_y_continuous(limits = extent[[2]]) +
    theme_void()
}

#' @export
plot.causal_model <- function(x, ...) {
  plot_model(x,...)
}


#' Alias
plot_dag <- plot_model


#' internal helper
#' adjust_edge moves the base and tip of edge arrows so as to avoid overlap
#' with nodes
#' @keywords internal

adjust_edge <- function(dag, nodesize) {
  dag <- tidyr::drop_na(dag)

  # Calculate the scale of the plot based on x and y range
  x_range <- range(c(dag$x, dag$xend), na.rm = TRUE)
  y_range <- range(c(dag$y, dag$yend), na.rm = TRUE)

  # Calculate the adjustment factor proportional to the plot size
  scale_factor <- mean(diff(x_range), diff(y_range)) / 100
  adjustment <- nodesize * scale_factor

  for(i in 1:nrow(dag)) {
    dx <- dag[i, "xend"] - dag[i, "x"]
    dy <- dag[i, "yend"] - dag[i, "y"]
    distance <- abs(dx) + abs(dy)

    if (distance == 0) {
      next
    }

    # Normalize the direction vector
    dir_x <- dx / distance
    dir_y <- dy / distance

    # Adjust positions
    dag[i, "x"] <- dag[i, "x"] + adjustment * dir_x
    dag[i, "y"] <- dag[i, "y"] + adjustment * dir_y
    dag[i, "xend"] <- dag[i, "xend"] - adjustment * dir_x
    dag[i, "yend"] <- dag[i, "yend"] - adjustment * dir_y
  }

  return(dag)
}


#' internal helper
#' calculates the attractive force component used in generating
#' force-directed graph layouts
#' @keywords internal

attractive_force <- function(distance, desired_distance, k) {
  return(k * (distance - desired_distance))
}

#' internal helper
#' calculates the repulsive force component used in generating
#' force directed graph layouts
#' @keywords internal

repulsive_force <- function(distance, k) {
  return((k^2) / distance)
}


#' internal helper
#' position_nodes generates a force directed graph layout to optimally
#' position nodes. Connected nodes attract while unconnected nodes repel.
#' position_nodes additionally ensures that connected nodes are not placed
#' vertically or horizontally of each other i.e. that the slope of the edge
#' connecting them is not 0 or inf.
#' @keywords internal

position_nodes <- function(dag,
                           nodes,
                           iterations = 1000L,
                           k = 0.5,
                           repulsion = 0.1,
                           desired_distance = 1) {
  # Generate adjacency matrix
  num_nodes <- length(nodes)
  parents <- match(dag$v, nodes)
  children <- match(dag$w, nodes)

  adj_matrix <- matrix(0, length(nodes), length(nodes))

  for(i in 1:length(nodes)) {
    adj_matrix[parents[i], children[i]] <- 1
    adj_matrix[children[i], parents[i]] <- 1
  }

  # Initialize node positions
  positions <- matrix(runif(num_nodes * 2), ncol = 2)

  # Force-directed algorithm
  for (iter in 1:iterations) {
    # Initialize the displacement matrix
    displacements <- matrix(0, nrow = num_nodes, ncol = 2)
    # Calculate forces between all pairs of nodes
    for (i in 1:num_nodes) {
      for (j in 1:num_nodes) {
        if (i != j) {
          delta <- positions[i, ] - positions[j, ]
          distance <- sqrt(sum(delta^2))

          # Calculate repulsive force
          force <- repulsive_force(distance, k)
          displacements[i, ] <- displacements[i, ] + (delta / distance) * force

          # Calculate attractive force if nodes are connected
          if (adj_matrix[i, j] == 1) {
            force <- attractive_force(distance, desired_distance, k)
            displacements[i, ] <- displacements[i, ] - (delta / distance) * force
          }
        }
      }
    }

    positions <- positions + displacements * repulsion

    # Post-process to ensure no horizontal or vertical edges
    for (i in 1:num_nodes) {
      for (j in 1:num_nodes) {
        if (adj_matrix[i, j] == 1) {
          delta <- positions[i, ] - positions[j, ]
          if (abs(delta[1]) < 0.5) {
            positions[i, 1] <- positions[i, 1] + runif(1, -0.1, 0.1)
          }
          if (abs(delta[2]) < 0.5) {
            positions[i, 2] <- positions[i, 2] + runif(1, -0.1, 0.1)
          }
        }
      }
    }
  }

  positions <- data.frame(
    node = nodes,
    x = positions[,1],
    y = positions[,2]
  )

  return(positions)
}


