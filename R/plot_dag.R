#' Plots a DAG in ggplot style using a causal model input
#'
#' Creates a plot of a DAG using ggplot functionality.  Unmeasured confounds  (<->) are indicated
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

  # generate coordinates
  statement <- model$statement
  nodes <- as.character(model$nodes)

  dag <- make_dag(statement) |>
    dplyr::rename(x = v, y = w) |>
    dplyr::mutate(weight = 1)

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
    dplyr::mutate(
      x = x - mean(coords$x),
      y = y - mean(coords$y))

  if (is.null(labels)) {
    coords$name <- nodes
  } else {
    coords$name <- labels
  }


  # Ordering labels
  .p <- dag  |> ggraph::ggraph(layout = "sugiyama")
  coords <- coords[match(.p$data$name, coords$node),]

  # buffer
  buffer_x <- 0.05 * (max(coords$x) - min(coords$x))
  buffer_y <- 0.05 * (max(coords$y) - min(coords$y))

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
      ggplot2::labs(title = latex2exp::TeX(title)) +
      xlim(min(coords$x) - buffer_x, max(coords$x) + buffer_x) +
      ylim(min(coords$y) - buffer_y, max(coords$y) + buffer_y)


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





## force directed graph layout -------------------------------------------------

#' internal helper
#' calculates the attractive force component used in generating
#' force-directed graph layouts
#' @keywords internal

attractive_force <- function(distance, desired_distance, k) {
  return(k * (distance - desired_distance))^2
}

#' internal helper
#' calculates the repulsive force component used in generating
#' force directed graph layouts
#' @keywords internal

repulsive_force <- function(distance, k) {
  if (distance < 0.001) {
    distance <- 0.001
  }
  return(min(k / distance^2, k))
}

#' internal helper
#' calculates the orientation of the point triplet (p, q, r)
#' this is used to check intersection between line segments
#' @keywords internal

orientation <- function(p, q, r) {
  val <- (q[2] - p[2]) * (r[1] - q[1]) - (q[1] - p[1]) * (r[2] - q[2])
  if (val == 0) return(0)  # co-linear
  if (val > 0) return(1)   # clockwise
  return(2)                # counterclockwise
}

#' internal helper
#' checks whether two edges intersect given the edge start and endpoints
#' this is used to apply penalties for edge crossing in the force directed
#' graph layout
#' @keywords internal

edges_intersect <- function(p1, p2, p3, p4) {

  o1 <- orientation(p1, p2, p3)
  o2 <- orientation(p1, p2, p4)
  o3 <- orientation(p3, p4, p1)
  o4 <- orientation(p3, p4, p2)

  # intersection case
  if (o1 != o2 && o3 != o4) {
    return(TRUE)
  }

  return(FALSE)
}


#' internal helper
#' position_nodes generates a force directed graph layout to optimally
#' position nodes. Connected nodes attract while unconnected nodes repel.
#' positions_nodes additionally attempts to minimize edge intersections
#' and maximize layout compactness.
#' @keywords internal

position_nodes <- function(dag,
                           nodes,
                           iterations = 1000L,
                           k = 1,
                           repulsion = 0.1,
                           desired_distance = 0.5,
                           crossing_penalty = 0.1,
                           gravity = 0.1) {
  # generate adjacency matrix
  num_nodes <- length(nodes)
  parents <- match(dag$v, nodes)
  children <- match(dag$w, nodes)

  adj_matrix <- matrix(0, length(nodes), length(nodes))

  for(i in 1:length(nodes)) {
    adj_matrix[parents[i], children[i]] <- 1
    adj_matrix[children[i], parents[i]] <- 1
  }

  # initialize node positions
  positions <- matrix(runif(num_nodes * 2), ncol = 2)

  # force-directed algorithm
  for (iter in 1:iterations) {
    # initialize the displacement matrix
    displacements <- matrix(0, nrow = num_nodes, ncol = 2)

    # calculate forces between all pairs of nodes
    for (i in 1:num_nodes) {
      for (j in 1:num_nodes) {
        if (i != j) {
          delta <- positions[i, ] - positions[j, ]
          distance <- sqrt(sum(delta^2))

          # calculate repulsive force
          force <- repulsive_force(distance, k)
          displacements[i, ] <- displacements[i, ] + (delta / distance) * force

          # calculate attractive force if nodes are connected
          if (adj_matrix[i, j] == 1) {
            force <- attractive_force(distance, desired_distance, k)
            displacements[i, ] <- displacements[i, ] - (delta / distance) * force
          }
        }
      }
    }

    # apply edge crossing penalty
    for (i in 1:(num_nodes - 1)) {
      for (j in (i + 1):num_nodes) {
        if (adj_matrix[i, j] == 1) {
          for (p in 1:(num_nodes - 1)) {
            for (q in (p + 1):num_nodes) {
              if (adj_matrix[p, q] == 1 && (i != p && j != q)) {
                # Check if edges (i, j) and (p, q) cross
                if (edges_intersect(positions[i, ], positions[j, ], positions[p, ], positions[q, ])) {
                  # Apply repulsive force between crossed edges
                  d_ij <- positions[i, ] - positions[j, ]
                  d_pq <- positions[p, ] - positions[q, ]

                  displacements[i,] <- displacements[i,] + (d_ij / sqrt(sum(d_ij^2))) * crossing_penalty
                  displacements[j,] <- displacements[j,] + (-d_ij / sqrt(sum(d_ij^2))) * crossing_penalty
                  displacements[p,] <- displacements[p,] + (d_pq / sqrt(sum(d_pq^2))) * crossing_penalty
                  displacements[q,] <- displacements[q,] + (-d_pq / sqrt(sum(d_pq^2))) * crossing_penalty
                }
              }
            }
          }
        }
      }
    }

    # apply gravity for compactness
    center <- colMeans(positions)
    for (i in 1:num_nodes) {
      delta <- positions[i, ] - center
      displacements[i, ] <- displacements[i, ] - delta * gravity
    }

    # update node positions
    positions <- positions + displacements * repulsion
  }

  positions <- data.frame(
    node = nodes,
    x = positions[,1],
    y = positions[,2]
  )

  return(positions)
}


