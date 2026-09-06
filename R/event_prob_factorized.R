#' Factorized event probabilities (parameters-only)
#'
#' Complete-data event probabilities from nodal lambdas. Unconfounded models
#' use a 2^n grid product (no P). Confounded models reuse the path-encoded
#' \code{parmap}/\code{map} construction (same as Stan).
#'
#' @keywords internal
#' @noRd
NULL

#' Max complete-data columns for factorized Stan prep (option override).
#'
#' Default \(2^{12}=4096\). Above this, prep refuses the full grid.
#' R-side \code{prob_event_ve} can still marginalize without materializing
#' \(2^n\) when few nodes are observed.
#'
#' @keywords internal
#' @noRd
factorized_grid_max <- function() {
  as.integer(getOption("CausalQueries.factorized_grid_max", 4096L))
}

#' Number of complete binary assignments (= columns of full-grid E / parmap).
#' @keywords internal
#' @noRd
n_complete_patterns <- function(model) {
  as.integer(2^length(model$nodes))
}

#' Error if full-grid Stan prep would exceed \code{factorized_grid_max()}.
#' @keywords internal
#' @noRd
check_factorized_grid_size <- function(model, what = "factorized prep") {
  n <- n_complete_patterns(model)
  cap <- factorized_grid_max()
  if (n <= cap) {
    return(invisible(n))
  }
  stop(
    what, ": complete-data grid has ", n, " columns (2^",
    length(model$nodes), "), over CausalQueries.factorized_grid_max=", cap,
    ". Use fewer nodes, set_restrictions / simplify_model, raise the option ",
    "for medium graphs, or use prob_event_ve() for R-side coarsened ",
    "probabilities without Stan prep.",
    call. = FALSE
  )
}

#' Whether the model statement declares confounding.
#' @keywords internal
#' @noRd
model_has_confound <- function(model) {
  grepl("<->", model$statement, fixed = TRUE)
}

#' Confound pairs from the model statement: list of c(later, earlier)
#' with later conditioned on earlier (same order as set_confound).
#' @keywords internal
#' @noRd
confound_pairs <- function(model) {
  if (!model_has_confound(model)) {
    return(list())
  }
  parts <- strsplit(model$statement, ";", fixed = TRUE)[[1]]
  parts <- trimws(parts)
  pairs <- list()
  for (p in parts) {
    if (!grepl("<->", p, fixed = TRUE)) {
      next
    }
    z <- trimws(strsplit(p, "<->", fixed = TRUE)[[1]])
    if (length(z) != 2L) {
      next
    }
    # Reverse causal order: later node conditioned on earlier
    ord <- rev(model$nodes[model$nodes %in% z])
    if (length(ord) != 2L) {
      next
    }
    pairs[[length(pairs) + 1L]] <- ord
  }
  pairs
}

#' Connected components of the confound graph (atomic blocks).
#' @keywords internal
#' @noRd
confound_components <- function(model) {
  nodes <- model$nodes
  parent <- setNames(seq_along(nodes), nodes)
  find <- function(i) {
    while (parent[[i]] != i) {
      parent[[i]] <<- parent[[parent[[i]]]]
      i <- parent[[i]]
    }
    i
  }
  union <- function(a, b) {
    ra <- find(match(a, nodes))
    rb <- find(match(b, nodes))
    if (ra != rb) {
      parent[[rb]] <<- ra
    }
  }
  for (pr in confound_pairs(model)) {
    union(pr[[1]], pr[[2]])
  }
  roots <- vapply(seq_along(nodes), find, integer(1))
  split(nodes, roots)
}

#' Index in a nodal-type string for a parent realization (0-based).
#' Matches realise_outcomes_c: pos = sum_k (1 << k) * parent_val[k]
#' with parents in get_parents() order.
#' @keywords internal
#' @noRd
nodal_type_parent_index <- function(parent_values) {
  if (length(parent_values) == 0L) {
    return(0L)
  }
  pv <- as.integer(parent_values)
  if (any(!pv %in% c(0L, 1L))) {
    stop("Parent values must be 0 or 1", call. = FALSE)
  }
  as.integer(sum(vapply(seq_along(pv), function(k) {
    bitwShiftL(1L, as.integer(k - 1L)) * pv[[k]]
  }, integer(1))))
}

#' Whether a nodal type is consistent with a complete assignment at `node`.
#' @keywords internal
#' @noRd
nodal_type_consistent <- function(model, node, nodal_type, assignment) {
  nodal_type <- as.character(nodal_type)
  parents <- get_parents(model)[[node]]
  if (length(parents) == 0L) {
    return(identical(nodal_type, as.character(assignment[[node]])))
  }
  parent_vals <- vapply(parents, function(p) as.integer(assignment[[p]]), integer(1))
  pos <- nodal_type_parent_index(parent_vals) + 1L
  if (nchar(nodal_type) < pos) {
    return(FALSE)
  }
  identical(substr(nodal_type, pos, pos), as.character(assignment[[node]]))
}

#' Probability mass on node `node` for one complete assignment (unconfounded).
#' @keywords internal
#' @noRd
nodal_assignment_prob <- function(model, parameters, node, assignment) {
  pdf <- model$parameters_df
  rows <- which(pdf$node == node)
  types <- as.character(pdf$nodal_type[rows])
  lambdas <- as.numeric(parameters[rows])
  ok <- vapply(types, function(t) {
    nodal_type_consistent(model, node, t, assignment)
  }, logical(1))
  sum(lambdas[ok])
}

#' Complete-data patterns without causal types (2^n grid).
#' @keywords internal
#' @noRd
complete_data_grid <- function(model) {
  get_all_data_types(model, complete_data = TRUE)
}

#' One complete assignment probability (unconfounded product or confound parmap).
#' @keywords internal
#' @noRd
complete_assignment_prob <- function(model, parameters, assignment,
                                     parmap = NULL) {
  assignment <- lapply(assignment, as.integer)
  if (!model_has_confound(model)) {
    p <- 1
    for (node in model$nodes) {
      p <- p * nodal_assignment_prob(model, parameters, node, assignment)
    }
    return(as.numeric(p))
  }
  if (is.null(parmap)) {
    parmap <- make_parmap_factorized(model)
  }
  grid <- complete_data_grid(model)
  nodes <- model$nodes
  ok <- rep(TRUE, nrow(grid))
  for (nm in nodes) {
    ok <- ok & (as.integer(grid[[nm]]) == as.integer(assignment[[nm]]))
  }
  j <- which(ok)
  if (length(j) != 1L) {
    stop("complete_assignment_prob: assignment not found on complete grid.",
         call. = FALSE)
  }
  # Paths -> data via map (confound may split paths); same as event_prob_from_parmap
  x <- rowsum(parmap * parameters,
              group = model$parameters_df$node,
              reorder = FALSE)
  w0 <- apply(x, 2, prod)
  map <- t(attr(parmap, "map"))
  w <- as.numeric(map %*% w0)
  data_names <- rownames(map)
  if (is.null(data_names)) {
    data_names <- as.character(grid$event)
  }
  hit <- match(as.character(grid$event[j]), data_names)
  if (is.na(hit)) {
    hit <- j
  }
  as.numeric(w[[hit]])
}

#' Event probabilities via parmap product (matches Stan / legacy).
#' @keywords internal
#' @noRd
event_prob_from_parmap <- function(model, parameters, parmap, given = NULL) {
  map <- t(attr(parmap, "map"))
  x <- rowsum(parmap * parameters,
              group = model$parameters_df$node,
              reorder = FALSE)
  x <- apply(x, 2, prod)
  event_probs <- map %*% x

  if (!is.null(given)) {
    types <- get_all_data_types(model, complete_data = TRUE)
    i <- match(rownames(event_probs), rownames(types))
    if (anyNA(i)) {
      stop("Event names do not match complete data types.", call. = FALSE)
    }
    matches <- with(types[i, , drop = FALSE], eval(parse(text = given)))
    matches[is.na(matches)] <- FALSE
    w <- as.numeric(event_probs)
    w[!matches] <- 0
    s <- sum(w)
    if (!(s > 0)) {
      stop("No probability mass matches `given`.", call. = FALSE)
    }
    event_probs[, 1] <- w / s
  }

  colnames(event_probs) <- "event_probs"
  class(event_probs) <- c("matrix", "array")
  event_probs
}

#' Whether a complete assignment is realizable under remaining nodal types.
#' Used to drop impossible data rows (match legacy ambiguities support).
#' @keywords internal
#' @noRd
complete_assignment_possible <- function(model, assignment) {
  assignment <- lapply(assignment, as.integer)
  pdf <- model$parameters_df
  for (node in model$nodes) {
    rows <- which(pdf$node == node)
    ok <- FALSE
    for (i in rows) {
      if (nodal_type_consistent(model, node, pdf$nodal_type[i], assignment)) {
        ok <- TRUE
        break
      }
    }
    if (!ok) {
      return(FALSE)
    }
  }
  TRUE
}

#' Event probabilities (factorized path; supports confound via parmap).
#'
#' @inheritParams CausalQueries_internal_inherit_params
#' @param given Optional conditioning string on complete-data columns.
#' @return One-column matrix of event probabilities (rownames = event names).
#' @keywords internal
#' @noRd
event_prob_factorized <- function(model,
                                 parameters = NULL,
                                 given = NULL) {
  is_a_model(model)

  if (!is.null(parameters)) {
    parameters <- clean_param_vector(model, parameters)
  } else {
    parameters <- get_parameters(model)
  }

  if (model_has_confound(model)) {
    return(event_prob_from_parmap(
      model, parameters, make_parmap_factorized(model), given = given
    ))
  }

  grid <- complete_data_grid(model)
  nodes <- model$nodes
  n_ev <- nrow(grid)
  probs <- numeric(n_ev)
  possible <- rep(TRUE, n_ev)

  for (i in seq_len(n_ev)) {
    assignment <- grid[i, nodes, drop = FALSE]
    possible[i] <- complete_assignment_possible(model, assignment)
    if (!possible[i]) {
      probs[i] <- 0
      next
    }
    p <- 1
    for (node in nodes) {
      p <- p * nodal_assignment_prob(model, parameters, node, assignment)
    }
    probs[i] <- p
  }

  names(probs) <- as.character(grid$event)
  # Drop impossible events so rownames match legacy ambiguities / possible data
  probs <- probs[possible]
  event_probs <- matrix(probs, ncol = 1,
                        dimnames = list(names(probs), "event_probs"))

  if (!is.null(given)) {
    matches <- with(grid[possible, , drop = FALSE], eval(parse(text = given)))
    matches[is.na(matches)] <- FALSE
    w <- as.numeric(event_probs)
    w[!matches] <- 0
    s <- sum(w)
    if (!(s > 0)) {
      stop("No probability mass matches `given`.", call. = FALSE)
    }
    event_probs[, 1] <- w / s
  }

  class(event_probs) <- c("matrix", "array")
  event_probs
}

#' Parameter-to-path incidence for Stan (unconfounded grid or legacy confound).
#'
#' Unconfounded: 2^n grid × nodal consistency; \code{map = I}.
#' Confounded: delegates to \code{make_parmap} (path split via P / given).
#'
#' @keywords internal
#' @noRd
make_parmap_factorized <- function(model) {
  if (model_has_confound(model)) {
    check_factorized_grid_size(model, "make_parmap_factorized")
    return(make_parmap(model))
  }

  check_factorized_grid_size(model, "make_parmap_factorized")
  grid <- complete_data_grid(model)
  nodes <- model$nodes
  pdf <- model$parameters_df
  n_params <- nrow(pdf)
  n_data <- nrow(grid)
  out <- matrix(0L, nrow = n_params, ncol = n_data)
  rownames(out) <- pdf$param_names
  colnames(out) <- as.character(grid$event)

  for (j in seq_len(n_data)) {
    assignment <- grid[j, nodes, drop = FALSE]
    for (i in seq_len(n_params)) {
      node <- pdf$node[i]
      if (nodal_type_consistent(model, node, pdf$nodal_type[i], assignment)) {
        out[i, j] <- 1L
      }
    }
  }

  map <- diag(n_data)
  rownames(map) <- colnames(map) <- colnames(out)
  attr(out, "map") <- map
  class(out) <- c("matrix", "array")
  out
}
