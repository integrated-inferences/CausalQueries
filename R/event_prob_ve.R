#' Factor-graph variable elimination for event probabilities
#'
#' Computes P(evidence) by eliminating unobserved nodes over nodal factors
#' (unconfounded) or a joint / latent-sum path under confounding.
#' Helpers also live in \code{event_prob_factorized.R}.
#' Stan prep still requires at most \code{factorized_grid_max()} complete-data
#' columns.
#'
#' @keywords internal
#' @noRd
NULL

#' Order nodes as in model$nodes (stable scopes).
#' @keywords internal
#' @noRd
nodes_in_model_order <- function(model, nodes) {
  model$nodes[model$nodes %in% unique(as.character(nodes))]
}

#' Bit index for a 0/1 configuration of \code{scope} (scope order = bits 0..).
#' @keywords internal
#' @noRd
config_index <- function(values, scope) {
  if (!length(scope)) {
    return(1L)
  }
  pv <- as.integer(values[scope])
  as.integer(sum(vapply(seq_along(pv), function(j) {
    bitwShiftL(1L, as.integer(j - 1L)) * pv[[j]]
  }, integer(1)))) + 1L
}

#' All 0/1 configurations of scope as a matrix (rows = configs).
#' @keywords internal
#' @noRd
scope_configs <- function(scope) {
  k <- length(scope)
  if (k == 0L) {
    return(matrix(integer(0), nrow = 1L, ncol = 0L))
  }
  g <- as.matrix(perm(rep(1, k)))
  colnames(g) <- scope
  g
}

#' Named 0/1 assignment from one config row.
#' @keywords internal
#' @noRd
row_assignment <- function(row, scope) {
  as.list(stats::setNames(as.integer(row), scope))
}

#' One factor: scope + values over 2^|scope| configs.
#' @keywords internal
#' @noRd
new_factor <- function(scope, values) {
  scope <- as.character(scope)
  n <- 2L^length(scope)
  if (length(values) != n) {
    stop("Factor values length must be 2^|scope|.", call. = FALSE)
  }
  list(scope = scope, values = as.numeric(values))
}

#' Nodal factor ψ_v(x_v, x_pa) for unconfounded models.
#' @keywords internal
#' @noRd
nodal_factor <- function(model, parameters, node) {
  parents <- get_parents(model)[[node]]
  scope <- nodes_in_model_order(model, c(parents, node))
  cfgs <- scope_configs(scope)
  vals <- numeric(nrow(cfgs))
  for (i in seq_len(nrow(cfgs))) {
    assignment <- row_assignment(cfgs[i, ], scope)
    vals[i] <- nodal_assignment_prob(model, parameters, node, assignment)
  }
  new_factor(scope, vals)
}

#' Factor graph for an unconfounded model.
#' @keywords internal
#' @noRd
factor_graph_unconfounded <- function(model, parameters) {
  if (model_has_confound(model)) {
    stop(
      "factor_graph_unconfounded: model has confounding; use factor_graph().",
      call. = FALSE
    )
  }
  parameters <- clean_param_vector(model, parameters)
  factors <- lapply(model$nodes, function(v) {
    nodal_factor(model, parameters, v)
  })
  names(factors) <- model$nodes
  list(
    model = model,
    parameters = parameters,
    factors = factors,
    nodes = model$nodes,
    parents = get_parents(model),
    confounded = FALSE
  )
}

#' Confound: one joint factor over all nodes from parmap (modest n).
#' Elimination then matches latent-sum / legacy coarsened marginals.
#' @keywords internal
#' @noRd
factor_graph_confounded <- function(model, parameters) {
  check_factorized_grid_size(model, "factor_graph (confound joint)")
  parameters <- clean_param_vector(model, parameters)
  parmap <- make_parmap_factorized(model)
  grid <- complete_data_grid(model)
  scope <- model$nodes
  vals <- numeric(2L^length(scope))
  for (i in seq_len(nrow(grid))) {
    a <- row_assignment(unlist(grid[i, scope, drop = FALSE]), scope)
    idx <- config_index(a, scope)
    vals[idx] <- complete_assignment_prob(
      model, parameters, a, parmap = parmap
    )
  }
  list(
    model = model,
    parameters = parameters,
    factors = list(joint = new_factor(scope, vals)),
    nodes = model$nodes,
    parents = get_parents(model),
    confounded = TRUE
  )
}

#' Build factor graph (unconfounded nodal factors; confound = joint factor).
#' @keywords internal
#' @noRd
factor_graph <- function(model, parameters = NULL) {
  is_a_model(model)
  if (is.null(parameters)) {
    parameters <- get_parameters(model)
  }
  parameters <- clean_param_vector(model, parameters)
  if (model_has_confound(model)) {
    return(factor_graph_confounded(model, parameters))
  }
  factor_graph_unconfounded(model, parameters)
}

#' Normalize evidence to named 0/1 list (drop NA / missing names).
#' @keywords internal
#' @noRd
normalize_evidence <- function(evidence, nodes) {
  evidence <- as.list(evidence)
  out <- list()
  for (nm in names(evidence)) {
    if (!nm %in% nodes) {
      stop("Unknown node in evidence: ", nm, call. = FALSE)
    }
    val <- evidence[[nm]]
    if (is.null(val) || length(val) != 1L || is.na(val)) {
      next
    }
    val <- as.integer(val)
    if (!val %in% c(0L, 1L)) {
      stop("Evidence values must be 0 or 1.", call. = FALSE)
    }
    out[[nm]] <- val
  }
  out
}

#' Confound coarsened prob: sum complete P over latent completions.
#' Avoids building a joint factor when \(2^{n_{\mathrm{lat}}}\) is enough.
#' @keywords internal
#' @noRd
prob_event_confound_latent_sum <- function(model, parameters, evidence) {
  ev <- normalize_evidence(evidence, model$nodes)
  latents <- setdiff(model$nodes, names(ev))
  n_lat <- as.integer(2^length(latents))
  if (n_lat > factorized_grid_max()) {
    stop(
      "prob_event_ve: 2^", length(latents), " latent completions exceeds ",
      "CausalQueries.factorized_grid_max=", factorized_grid_max(),
      " under confounding.",
      call. = FALSE
    )
  }
  # Full parmap still needs complete grid columns
  check_factorized_grid_size(model, "prob_event_ve confound")
  parmap <- make_parmap_factorized(model)
  if (!length(latents)) {
    return(complete_assignment_prob(model, parameters, ev, parmap = parmap))
  }
  cfgs <- scope_configs(latents)
  s <- 0
  for (i in seq_len(nrow(cfgs))) {
    a <- c(ev, row_assignment(cfgs[i, ], latents))
    s <- s + complete_assignment_prob(model, parameters, a, parmap = parmap)
  }
  as.numeric(s)
}

#' Probability of one complete assignment = product of nodal factors.
#' @keywords internal
#' @noRd
prob_complete_from_graph <- function(graph, assignment) {
  assignment <- lapply(assignment, as.integer)
  p <- 1
  for (fac in graph$factors) {
    idx <- config_index(assignment, fac$scope)
    p <- p * fac$values[[idx]]
  }
  p
}

#' Restrict a factor to evidence (observed nodes).
#' @keywords internal
#' @noRd
clamp_factor <- function(fac, evidence) {
  scope <- fac$scope
  obs <- intersect(scope, names(evidence)[!is.na(evidence)])
  if (!length(obs)) {
    return(fac)
  }
  keep_scope <- setdiff(scope, obs)
  cfgs <- scope_configs(scope)
  vals <- fac$values
  ok <- rep(TRUE, nrow(cfgs))
  for (o in obs) {
    j <- match(o, scope)
    ok <- ok & (cfgs[, j] == as.integer(evidence[[o]]))
  }
  if (!length(keep_scope)) {
    return(new_factor(character(0), sum(vals[ok])))
  }
  out_cfgs <- scope_configs(keep_scope)
  out_vals <- numeric(nrow(out_cfgs))
  # Map each surviving row to reduced config
  for (i in which(ok)) {
    red <- row_assignment(cfgs[i, keep_scope], keep_scope)
    idx <- config_index(red, keep_scope)
    out_vals[idx] <- out_vals[idx] + vals[i]
  }
  new_factor(keep_scope, out_vals)
}

#' Multiply two factors (scope order = f1 then new vars from f2).
#' @keywords internal
#' @noRd
multiply_factors <- function(f1, f2) {
  scope <- unique(c(f1$scope, f2$scope))
  cfgs <- scope_configs(scope)
  vals <- numeric(nrow(cfgs))
  for (i in seq_len(nrow(cfgs))) {
    a <- row_assignment(cfgs[i, ], scope)
    vals[i] <- f1$values[[config_index(a, f1$scope)]] *
      f2$values[[config_index(a, f2$scope)]]
  }
  new_factor(scope, vals)
}

#' Sum out one variable from a factor.
#' @keywords internal
#' @noRd
sum_out_factor <- function(fac, var) {
  if (!var %in% fac$scope) {
    return(fac)
  }
  keep <- setdiff(fac$scope, var)
  if (!length(keep)) {
    return(new_factor(character(0), sum(fac$values)))
  }
  cfgs <- scope_configs(fac$scope)
  out_vals <- numeric(2L^length(keep))
  for (i in seq_len(nrow(cfgs))) {
    red <- row_assignment(cfgs[i, keep], keep)
    idx <- config_index(red, keep)
    out_vals[idx] <- out_vals[idx] + fac$values[[i]]
  }
  new_factor(keep, out_vals)
}

#' Reverse-topological elimination order among latents.
#' @keywords internal
#' @noRd
elimination_order_latents <- function(model, latents) {
  latents <- nodes_in_model_order(model, latents)
  # model$nodes is already causal order; eliminate children before parents
  rev(latents)
}

#' Variable elimination: product of factors, sum out latents.
#'
#' @param evidence Named list/vector of 0/1 for observed nodes; omit or NA = latent.
#' @return Scalar probability of the evidence (marginal).
#' @keywords internal
#' @noRd
eliminate_evidence <- function(graph, evidence) {
  evidence <- as.list(evidence)
  # Normalize: only 0/1 kept as observed
  obs <- character(0)
  ev <- list()
  for (nm in names(evidence)) {
    val <- evidence[[nm]]
    if (is.null(val) || length(val) != 1L || is.na(val)) {
      next
    }
    val <- as.integer(val)
    if (!val %in% c(0L, 1L)) {
      stop("Evidence values must be 0 or 1.", call. = FALSE)
    }
    if (!nm %in% graph$nodes) {
      stop("Unknown node in evidence: ", nm, call. = FALSE)
    }
    obs <- c(obs, nm)
    ev[[nm]] <- val
  }
  latents <- setdiff(graph$nodes, obs)

  factors <- lapply(graph$factors, clamp_factor, evidence = ev)

  for (v in elimination_order_latents(graph$model, latents)) {
    hit <- which(vapply(factors, function(f) v %in% f$scope, logical(1)))
    if (!length(hit)) {
      next
    }
    combined <- factors[[hit[1]]]
    if (length(hit) > 1L) {
      for (h in hit[-1]) {
        combined <- multiply_factors(combined, factors[[h]])
      }
    }
    factors <- factors[-hit]
    factors[[length(factors) + 1L]] <- sum_out_factor(combined, v)
  }

  # Remaining factors should be empty-scope constants (or none)
  p <- 1
  for (f in factors) {
    if (length(f$scope)) {
      # Should not happen if all latents eliminated; sum remaining
      p <- p * sum(f$values)
    } else {
      p <- p * f$values[[1]]
    }
  }
  as.numeric(p)
}

#' Probability of evidence via VE (unconfounded factor VE; confound latent sum).
#'
#' @param model A causal_model.
#' @param parameters Parameter vector.
#' @param evidence Named 0/1 values for observed nodes (others marginalized).
#' @return Scalar in [0, 1].
#' @keywords internal
#' @noRd
prob_event_ve <- function(model, parameters = NULL, evidence) {
  is_a_model(model)
  if (is.null(parameters)) {
    parameters <- get_parameters(model)
  }
  parameters <- clean_param_vector(model, parameters)
  if (model_has_confound(model)) {
    return(prob_event_confound_latent_sum(model, parameters, evidence))
  }
  graph <- factor_graph_unconfounded(model, parameters)
  eliminate_evidence(graph, evidence)
}

#' Full event probability vector via VE / latent sum (parity helper).
#' @keywords internal
#' @noRd
event_prob_ve_complete <- function(model, parameters = NULL) {
  if (is.null(parameters)) {
    parameters <- get_parameters(model)
  }
  parameters <- clean_param_vector(model, parameters)
  grid <- complete_data_grid(model)
  nodes <- model$nodes
  probs <- numeric(nrow(grid))
  if (model_has_confound(model)) {
    parmap <- make_parmap_factorized(model)
    for (i in seq_len(nrow(grid))) {
      ev <- row_assignment(unlist(grid[i, nodes, drop = FALSE]), nodes)
      probs[i] <- complete_assignment_prob(
        model, parameters, ev, parmap = parmap
      )
    }
  } else {
    graph <- factor_graph_unconfounded(model, parameters)
    for (i in seq_len(nrow(grid))) {
      ev <- row_assignment(unlist(grid[i, nodes, drop = FALSE]), nodes)
      probs[i] <- eliminate_evidence(graph, ev)
    }
  }
  names(probs) <- as.character(grid$event)
  matrix(probs, ncol = 1L, dimnames = list(names(probs), "event_probs"))
}

#' Sum complete-data event probs matching a partial assignment (parity oracle).
#' Uses full-grid \code{event_prob_factorized} (or legacy via that path).
#' @keywords internal
#' @noRd
prob_coarsened_from_grid <- function(model, parameters, evidence) {
  evidence <- as.list(evidence)
  evidence <- evidence[!vapply(evidence, function(x) {
    length(x) != 1L || is.na(x)
  }, logical(1))]
  w <- event_prob_factorized(model, parameters = parameters)
  grid <- complete_data_grid(model)
  keep <- rep(TRUE, nrow(grid))
  for (nm in names(evidence)) {
    keep <- keep & (as.integer(grid[[nm]]) == as.integer(evidence[[nm]]))
  }
  sum(as.numeric(w)[keep])
}

#' Parse a simple observational \code{given} string into evidence.
#'
#' Accepts conjunctions of \code{Node==0} / \code{Node==1} (optional spaces).
#' Returns \code{NULL} if the string is not a pure observational conjunction
#' (e.g. contains do-brackets or inequalities) — caller should use type path.
#'
#' @keywords internal
#' @noRd
parse_observational_evidence <- function(given) {
  if (is.null(given) || isTRUE(given)) {
    return(list())
  }
  g <- trimws(as.character(given))
  if (!nzchar(g) || g %in% c("ALL", "TRUE")) {
    return(list())
  }
  if (grepl("\\[|\\]|<|>|\\||!=", g)) {
    return(NULL)
  }
  parts <- strsplit(g, "&", fixed = TRUE)[[1]]
  out <- list()
  for (p in parts) {
    p <- trimws(p)
    m <- regexec("^([A-Za-z][A-Za-z0-9_]*)\\s*==\\s*([01])$", p)
    r <- regmatches(p, m)[[1]]
    if (length(r) != 3L) {
      return(NULL)
    }
    out[[r[[2]]]] <- as.integer(r[[3]])
  }
  out
}

#' Mass of an observational \code{given} via data VE.
#' @keywords internal
#' @noRd
prob_given_ve <- function(model, parameters = NULL, given) {
  ev <- parse_observational_evidence(given)
  if (is.null(ev)) {
    stop(
      "prob_given_ve: given is not a pure observational conjunction of Node==0/1.",
      call. = FALSE
    )
  }
  prob_event_ve(model, parameters = parameters, evidence = ev)
}
