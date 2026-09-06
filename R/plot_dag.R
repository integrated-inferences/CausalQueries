#' Plots a DAG in ggplot style using a causal model input
#'
#' Creates a plot of a DAG using ggplot and a Sugiyama layout from igraph
#' (via ggraph). Unmeasured confounds (`<->`) are drawn as dashed arcs.
#' Users can control node sizes, colors, coordinates, and label behavior.
#' Other modifications can be made by adding ggplot layers.
#'
#' @param model A \code{causal_model} object generated from \code{make_model}
#' @param x_coord A vector of x coordinates for DAG nodes, in the same order
#'   as \code{model$nodes}. If both \code{x_coord} and \code{y_coord} are
#'   \code{NULL}, a Sugiyama layout is computed from directed edges only.
#' @param y_coord A vector of y coordinates for DAG nodes, in the same order
#'   as \code{model$nodes}.
#' @param labels Optional labels for nodes
#' @param title String specifying title of graph
#' @param textcol String specifying color of text labels
#' @param textsize Numeric, size of text labels
#' @param shape Indicates shape of node. Defaults to circular node.
#' @param nodecol String indicating color of node that is accepted by
#'   ggplot's default palette
#' @param nodesize Size of node.
#' @param parse Logical. If `TRUE`, node labels are parsed as R expressions,
#'   which allows mathematical notation (for example `"alpha^2"`).
#'   Defaults to `FALSE`.
#' @param strength Curvature of confound arcs, passed to
#'   \code{\link[ggplot2]{geom_curve}} (\code{0} = straight,
#'   about \code{0.3} = a shallow bow, \code{1} ≈ a semicircle). Use
#'   \code{NULL} or \code{"auto"} (the default) to choose a shallow
#'   signed curvature from geometry (see \code{confound_bulge}). A numeric
#'   value restores a fixed curvature (the old ggraph default of \code{0.3}
#'   is a reasonable shallow arc under \code{geom_curve}).
#' @param pad Additive scale padding in data units around nodes. \code{NULL}
#'   (default) chooses a pad from layer spacing so nodes are not clipped when
#'   several share an x or y coordinate (ggplot's multiplicative expand alone
#'   is zero in that case). Set to \code{0} to disable.
#' @param normalize_layout Logical. If \code{TRUE} (default) and coordinates
#'   were not supplied by the user, rescale x so horizontal span matches
#'   typical layer spacing. Prevents a tiny x-range from stretching across the
#'   whole panel (e.g. a sink node at \code{x = 0.5} while others sit at
#'   \code{x = 0}).
#' @param confound_bulge Absolute \code{geom_curve} curvature used when
#'   \code{strength} is \code{NULL}/\code{"auto"} (sign chosen to bend toward
#'   free space). Default \code{0.3}. Ignored when \code{strength} is numeric.
#' @param clip Passed to \code{\link[ggplot2]{coord_cartesian}}. Default
#'   \code{"off"} so node discs are not cropped at the panel edge.
#' @return A ggplot object.
#'
#' @import dplyr
#' @import ggplot2
#' @import ggraph
#' @importFrom grid arrow
#' @importFrom grid unit
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
#'   ggplot2::coord_flip()
#'
#' # Adding labels
#' model |>
#'   plot_model(
#'     labels = c("A long name for a \n node", "This", "That"),
#'     nodecol = "white",
#'     textcol = "black")
#'
#' # Math labels: add parse = TRUE
#' make_model() |>
#'   plot_model(
#'     labels = c("alpha^2", "beta[1]"),
#'     parse = TRUE,
#'     textcol  = "black", nodecol = "white")
#'
#' # Mixed math text labels: add parse = TRUE
#' make_model() |>
#'   plot_model(
#'       labels = c('alpha ~ "class"', 'beta ~ "class"'),
#'       parse = TRUE,
#'       textcol  = "black", nodecol = "white",)
#'
#' # Adding math title after graph creation
#' make_model() |>
#'   plot_model() +
#'   ggplot2::labs(title = expression(paste(Gamma, " graph")))
#' }
#'
#' # DAG with unobserved confounding and shapes and position control
#' make_model('Z -> X -> Y; X <-> Y') |>
#'   plot(x_coord = 1:3, y_coord = 1:3, shape = c(15, 16, 16))
#'
#' # Legacy confound curvature (fixed strength)
#' make_model('X -> M -> Y <-> M') |>
#'   plot_model(strength = 0.3, normalize_layout = FALSE, pad = 0)
#'
#' # Manual pad / bulge
#' make_model('X -> M -> Y <-> M') |>
#'   plot_model(pad = 0.5, confound_bulge = 0.25)


plot_model <- function(model = NULL,
                       x_coord = NULL,
                       y_coord = NULL,
                       labels = NULL,
                       title = "",
                       textcol = "white",
                       textsize = 3.88,
                       shape = 16,
                       nodecol = "black",
                       nodesize = 12,
                       parse = FALSE,
                       strength = NULL,
                       pad = NULL,
                       normalize_layout = TRUE,
                       confound_bulge = 0.3,
                       clip = "off") {
  if (is.null(model)) {
    stop("Model object must be provided")
  }

  if (!is(model, "causal_model")) {
    stop("Model object must be of type causal_model")
  }

  if (is.null(x_coord) == !is.null(y_coord)) {
    message("Coordinates should be provided for both x and y (or neither).")
    x_coord <- NULL
    y_coord <- NULL
  }

  if (!is.null(x_coord) &&
      !is.null(y_coord) &&
      length(x_coord) != length(y_coord)) {
    stop("x and y coordinates must be of equal length")
  }

  if (!is.null(x_coord) &&
      !is.null(y_coord) &&
      length(model$nodes) != length(x_coord)) {
    stop("length of coordinates supplied must equal number of nodes")
  }

  if (!is.null(labels) &&
      (length(model$nodes) != length(labels))) {
    stop("length of labels supplied must equal number of nodes")
  }

  user_coords <- !is.null(x_coord) && !is.null(y_coord)

  # Edge list: columns x/y hold endpoint *names* (ggraph convention here)
  dag <-
    model$statement |>
    make_dag() |>
    dplyr::rename(x = v, y = w) |>
    dplyr::mutate(weight = 1)

  if (nrow(dag) == 1 && all(is.na(dag$e))) {
    id_coords <- data.frame(x = 0, y = 0, name = dag$x, stringsAsFactors = FALSE)
    dag$e <- dag$y <- "NA"
  } else {
    id_coords <- layout_dag_coords(dag)
  }

  .r <- match(id_coords$name, model$nodes)
  r <- function(z) z[.r]

  if (!is.null(x_coord)) {
    id_coords$x <- r(x_coord)
  }
  if (!is.null(y_coord)) {
    id_coords$y <- r(y_coord)
  }

  if (!user_coords && isTRUE(normalize_layout)) {
    id_coords <- normalize_dag_coords(id_coords)
  }

  display <- id_coords
  if (!is.null(labels)) {
    display$name <- r(labels)
  }
  if (length(shape) > 1) {
    shape <- r(shape)
  }
  if (length(nodecol) > 1) {
    nodecol <- r(nodecol)
  }
  if (length(nodesize) > 1) {
    nodesize <- r(nodesize)
  }
  if (length(textcol) > 1) {
    textcol <- r(textcol)
  }
  if (length(textsize) > 1) {
    textsize <- r(textsize)
  }

  pos <- data.frame(
    x = id_coords$x,
    y = id_coords$y,
    row.names = id_coords$name,
    stringsAsFactors = FALSE
  )

  y_step <- layer_step(id_coords$y)
  if (is.null(pad)) {
    # Additive expand must be large enough in *data* units; node discs are
    # drawn in mm, so also rely on plot.margin below.
    pad <- max(0.315, 0.315 * y_step)
  }

  # Classify directed edges: same-layer-step links vs multi-layer skips
  # (skip edges drawn as links are invisible on a vertical chain, e.g. A -> D).
  dag$plot_edge <- NA_character_
  for (i in seq_len(nrow(dag))) {
    if (is.na(dag$e[i])) {
      next
    }
    if (dag$e[i] == "<->") {
      dag$plot_edge[i] <- "confound"
      next
    }
    if (dag$e[i] != "->") {
      next
    }
    a <- as.character(dag$x[i])
    b <- as.character(dag$y[i])
    if (!all(c(a, b) %in% rownames(pos))) {
      dag$plot_edge[i] <- "link"
      next
    }
    dy <- abs(pos[a, "y"] - pos[b, "y"])
    dx <- abs(pos[a, "x"] - pos[b, "x"])
    # Only bow edges that would otherwise sit on top of a vertical chain
    dag$plot_edge[i] <- if (dy > y_step * 1.25 && dx < 0.25 * y_step) {
      "skip"
    } else {
      "link"
    }
  }

  # Arc bend for confounds / skips (ggraph strength; keep modest — 1 is a semicircle).
  curv_vec <- confound_curve_curvatures(
    dag = dag,
    pos = pos,
    strength = strength,
    confound_bulge = confound_bulge
  )
  conf_idx <- which(dag$plot_edge == "confound")
  conf_strength <- if (length(conf_idx)) {
    sv <- curv_vec[conf_idx]
    sv[which.max(abs(sv))]
  } else {
    -0.3
  }

  skip_idx <- which(dag$plot_edge == "skip")
  skip_strength <- if (length(skip_idx)) {
    a <- as.character(dag$x[skip_idx[1]])
    b <- as.character(dag$y[skip_idx[1]])
    edge_mid <- mean(c(pos[a, "x"], pos[b, "x"]))
    s <- 0.22
    if (mean(pos$x) > edge_mid) {
      s <- -s
    }
    s
  } else {
    0.22
  }

  layout_names <- unique(c(as.character(dag$x), as.character(dag$y)))
  layout_names <- layout_names[!is.na(layout_names) & layout_names != "NA"]
  ord <- match(layout_names, id_coords$name)
  if (anyNA(ord)) {
    stop("Internal plot_model error: layout nodes missing coordinates")
  }

  # Margin must exceed node radius (nodesize is mm).
  plot_margin <- grid::unit(c(0.525, 0.525, 0.525, 0.525), "cm")
  # Dock edges with ggraph mm caps (not data-space shortening): paths stay
  # centre-to-centre; caps stop drawing at an absolute distance from the node.
  # Near-zero start_cap + nodes drawn on top => shaft appears to leave the disc
  # with no gap. end_cap ≈ radius + tip gap => arrowhead sits just outside.
  node_r_mm <- max(nodesize) * 0.5
  start_cap <- ggraph::circle(0.4, "mm")
  end_cap <- ggraph::circle(node_r_mm + 1.2, "mm")
  # Confounds have no arrow tip; stop near the rim on both ends.
  conf_cap <- ggraph::circle(node_r_mm * 0.92, "mm")
  arrow_len <- grid::unit(max(2, max(nodesize) * 0.18), "mm")
  edge_arrow <- grid::arrow(length = arrow_len, type = "closed")

  dag |>
    ggraph::ggraph(layout = "manual", x = id_coords$x[ord], y = id_coords$y[ord]) +
    ggraph::geom_edge_arc(
      data = edge_selector(function(e) e$plot_edge == "confound"),
      start_cap = conf_cap,
      end_cap = conf_cap,
      linetype = "dashed",
      strength = conf_strength
    ) +
    ggraph::geom_edge_arc(
      data = edge_selector(function(e) e$plot_edge == "skip"),
      arrow = edge_arrow,
      start_cap = start_cap,
      end_cap = end_cap,
      strength = skip_strength
    ) +
    ggraph::geom_edge_link(
      data = edge_selector(function(e) e$plot_edge == "link"),
      arrow = edge_arrow,
      start_cap = start_cap,
      end_cap = end_cap
    ) +
    ggplot2::geom_point(
      data = display,
      ggplot2::aes(x, y),
      size = nodesize,
      color = nodecol,
      shape = shape
    ) +
    ggplot2::theme_void() +
    ggplot2::theme(plot.margin = plot_margin) +
    ggraph::geom_node_text(
      data = display,
      ggplot2::aes(x, y, label = name),
      color = textcol,
      size = textsize,
      parse = parse
    ) +
    ggplot2::labs(title = title) +
    ggplot2::scale_x_continuous(
      expand = ggplot2::expansion(mult = 0.05, add = pad)
    ) +
    ggplot2::scale_y_continuous(
      expand = ggplot2::expansion(mult = 0.05, add = pad)
    ) +
    ggplot2::coord_cartesian(clip = clip)
}

#' Plot method for a causal model
#'
#' A \code{plot} method for objects of class \code{causal_model}; a thin
#' wrapper around \code{\link{plot_model}}.
#'
#' @param x A \code{causal_model} object generated from \code{make_model}.
#' @param ... Arguments passed to \code{\link{plot_model}}.
#' @return A ggplot object.
#'
#' @rdname plot_model
#' @export
plot.causal_model <- function(x, ...) {
  plot_model(x, ...)
}


## helpers -----------------------------------------------------------------

#' @keywords internal
edge_selector <- function(predicate) {
  function(layout) {
    edges <- get_edges()(layout)
    edges[predicate(edges), , drop = FALSE]
  }
}

#' @keywords internal
arc_selector <- function(x) {
  edge_selector(function(edges) edges$e == x)
}

#' Sugiyama coordinates from directed edges; include confound-only nodes.
#' @keywords internal
layout_dag_coords <- function(dag) {
  directed <- dag[!is.na(dag$e) & dag$e != "<->", , drop = FALSE]
  all_nodes <- unique(c(as.character(dag$x), as.character(dag$y)))
  all_nodes <- all_nodes[!is.na(all_nodes)]

  if (nrow(directed) == 0) {
    n <- length(all_nodes)
    return(data.frame(
      name = all_nodes,
      x = seq_len(n) - 1,
      y = rep(0, n),
      stringsAsFactors = FALSE
    ))
  }

  coords <- (directed |>
               ggraph::ggraph(layout = "sugiyama"))$data |>
    dplyr::select(x, y, name)

  missing <- setdiff(all_nodes, coords$name)
  if (length(missing)) {
    # Place leftover nodes to the side of the directed layout
    x_max <- max(coords$x)
    y_mid <- mean(range(coords$y))
    extra <- data.frame(
      name = missing,
      x = x_max + seq_along(missing),
      y = y_mid,
      stringsAsFactors = FALSE
    )
    coords <- dplyr::bind_rows(coords, extra)
  }

  as.data.frame(coords, stringsAsFactors = FALSE)
}

#' Rescale x so a tiny horizontal span does not dominate the panel.
#' @keywords internal
normalize_dag_coords <- function(coords) {
  x <- coords$x
  y <- coords$y
  rx <- diff(range(x))
  ry <- diff(range(y))
  y_step <- layer_step(y)

  if (rx < .Machine$double.eps) {
    return(coords)
  }

  ux <- sort(unique(x))
  target_rx <- y_step * max(1, length(ux) - 1)
  # If y is degenerate, fall back to unit spacing
  if (ry < .Machine$double.eps) {
    target_rx <- max(1, length(ux) - 1)
  }

  x_mid <- mean(range(x))
  coords$x <- (x - x_mid) / rx * target_rx
  coords
}

#' @keywords internal
layer_step <- function(y) {
  uy <- sort(unique(y))
  if (length(uy) >= 2) {
    return(stats::median(diff(uy)))
  }
  1
}

#' Pull geom_curve / geom_segment endpoints off node centres toward the rim.
#' @keywords internal
shorten_curve_ends <- function(df, rim = 0.2, rim_start = rim, rim_end = rim) {
  if (is.null(df) || !nrow(df)) {
    return(df)
  }
  dx <- df$xend - df$x
  dy <- df$yend - df$y
  len <- sqrt(dx * dx + dy * dy)
  # Never consume more than 40% of the chord at each end.
  s0 <- pmin(rim_start / pmax(len, 1e-9), 0.4)
  s1 <- pmin(rim_end / pmax(len, 1e-9), 0.4)
  df$x <- df$x + s0 * dx
  df$y <- df$y + s0 * dy
  df$xend <- df$xend - s1 * dx
  df$yend <- df$yend - s1 * dy
  df
}

#' Segment endpoints for ggplot2::geom_curve layers.
#' @keywords internal
curve_segment_df <- function(dag, pos, kind) {
  idx <- which(dag$plot_edge == kind)
  if (!length(idx)) {
    return(NULL)
  }
  rows <- lapply(idx, function(i) {
    a <- as.character(dag$x[i])
    b <- as.character(dag$y[i])
    data.frame(
      x = pos[a, "x"],
      y = pos[a, "y"],
      xend = pos[b, "x"],
      yend = pos[b, "y"],
      stringsAsFactors = FALSE
    )
  })
  dplyr::bind_rows(rows)
}

#' Signed geom_curve curvatures for confound edges.
#' @keywords internal
confound_curve_curvatures <- function(dag, pos, strength, confound_bulge) {
  n <- nrow(dag)
  out <- rep(0, n)
  auto <- is.null(strength) ||
    (is.character(strength) && length(strength) == 1 &&
       tolower(strength) %in% c("auto", "a"))

  if (!auto) {
    if (!is.numeric(strength) || length(strength) != 1 || is.na(strength)) {
      stop("`strength` must be NULL, \"auto\", or a single number")
    }
    out[!is.na(dag$e) & dag$e == "<->"] <- strength
    return(out)
  }

  if (!is.numeric(confound_bulge) || length(confound_bulge) != 1 ||
      is.na(confound_bulge) || confound_bulge < 0) {
    stop("`confound_bulge` must be a single non-negative number")
  }

  # geom_curve curvature is chord-relative; clamp to a shallow bow.
  mag <- min(max(confound_bulge, 0.12), 0.55)

  for (i in seq_len(n)) {
    if (is.na(dag$e[i]) || dag$e[i] != "<->") {
      next
    }
    a <- as.character(dag$x[i])
    b <- as.character(dag$y[i])
    if (!all(c(a, b) %in% rownames(pos))) {
      out[i] <- -mag
      next
    }
    x1 <- pos[a, "x"]
    y1 <- pos[a, "y"]
    x2 <- pos[b, "x"]
    y2 <- pos[b, "y"]
    s <- mag

    # Bend toward free space (away from other nodes' mean x)
    mid_x <- (x1 + x2) / 2
    others <- setdiff(rownames(pos), c(a, b))
    if (length(others)) {
      if (mean(pos[others, "x"]) > mid_x) {
        s <- -s
      }
    } else {
      s <- -s
    }
    out[i] <- s
  }
  out
}

# Back-compatible alias used by older tests / callers
#' @keywords internal
confound_arc_strengths <- confound_curve_curvatures
