#' Twin-network / block VE for factorized queries
#'
#' Factorized-only evaluator (\code{query_eval = "ve"} / \code{"auto"}).
#' Accumulates the same estimand as the grid path by chunking the relevant
#' type product and calling existing \code{realise_outcomes} /
#' \code{map_query_to_causal_type} / weight rules on each chunk (nested-do
#' safe). Not \code{event_prob_ve} (observational binary-node VE).
#'
#' @keywords internal
#' @noRd
NULL

factorized_query_max_types <- function() {
  as.numeric(getOption("CausalQueries.factorized_query_max", 1e6))
}

factorized_ve_max_types <- function() {
  as.numeric(getOption("CausalQueries.factorized_ve_max", 1e7))
}

factorized_ve_chunk_size <- function() {
  as.integer(getOption("CausalQueries.factorized_ve_chunk", 5000L))
}

default_query_eval <- function() {
  opt <- getOption("CausalQueries.query_eval", "grid")
  normalize_query_eval(opt)
}

normalize_query_eval <- function(query_eval) {
  if (is.null(query_eval) || length(query_eval) != 1L || is.na(query_eval)) {
    stop(
      "`query_eval` must be one of \"grid\", \"ve\", \"ve_struct\", \"auto\"",
      call. = FALSE
    )
  }
  query_eval <- as.character(query_eval)
  if (!query_eval %in% c("grid", "ve", "ve_struct", "auto")) {
    stop(
      "`query_eval` must be one of \"grid\", \"ve\", \"ve_struct\", \"auto\"",
      call. = FALSE
    )
  }
  query_eval
}

#' Resolve factorized query evaluator (ignored when legacy = TRUE).
#' @keywords internal
#' @noRd
resolve_query_eval <- function(query_eval = NULL) {
  if (is.null(query_eval)) {
    return(default_query_eval())
  }
  normalize_query_eval(query_eval)
}

#' Select grid vs VE / ve_struct for one factorized query.
#' @keywords internal
#' @noRd
choose_factorized_query_eval <- function(model, query, given, query_eval) {
  query_eval <- resolve_query_eval(query_eval)
  type_nodes <- query_type_nodes(model, query, given)
  n_hat <- estimate_relevant_type_product(model, type_nodes)
  max_types <- factorized_query_max_types()
  if (identical(query_eval, "grid")) {
    return(list(method = "grid", type_nodes = type_nodes, n_hat = n_hat))
  }
  if (identical(query_eval, "ve")) {
    return(list(method = "ve", type_nodes = type_nodes, n_hat = n_hat))
  }
  if (identical(query_eval, "ve_struct")) {
    return(list(method = "ve_struct", type_nodes = type_nodes, n_hat = n_hat))
  }
  # auto: chunked ve only when grid would refuse — never ve_struct
  if (!is.finite(n_hat) || n_hat > max_types) {
    list(method = "ve", type_nodes = type_nodes, n_hat = n_hat)
  } else {
    list(method = "grid", type_nodes = type_nodes, n_hat = n_hat)
  }
}

#' Run estimands for a chosen factorized method (with ve_struct fallback).
#' @keywords internal
#' @noRd
estimands_factorized_dispatch <- function(model,
                                          query,
                                          given,
                                          param_mat,
                                          method,
                                          type_nodes,
                                          join_by = "|",
                                          case_level = FALSE,
                                          using = "parameters") {
  if (identical(method, "ve_struct")) {
    admit <- twin_world_admit(model, query, given, confound_supported = TRUE)
    if (isTRUE(admit$ok)) {
      return(estimands_from_lambda_draws_ve_struct(
        model = model,
        admit = admit,
        query = query,
        given = given,
        param_mat = param_mat,
        join_by = join_by,
        case_level = case_level,
        using = using
      ))
    }
    # Visible fallback (lock 4)
    n_hat <- estimate_relevant_type_product(model, type_nodes)
    max_types <- factorized_query_max_types()
    fallback <- if (!is.finite(n_hat) || n_hat > max_types) "ve" else "grid"
    message_ve_struct_fallback(admit$reason, fallback)
    method <- fallback
  }

  if (identical(method, "ve")) {
    return(estimands_from_lambda_draws_ve(
      model = model,
      type_nodes = type_nodes,
      query = query,
      given = given,
      param_mat = param_mat,
      join_by = join_by,
      case_level = case_level,
      using = using
    ))
  }

  schedule <- factorized_query_schedule(model, query = query, given = given)
  estimands_from_lambda_draws(
    model = model,
    schedule = schedule,
    query = query,
    given = given,
    param_mat = param_mat,
    join_by = join_by,
    case_level = case_level,
    using = using
  )
}

#' Stop with a clear product message for VE.
#' @keywords internal
#' @noRd
stop_ve_product_too_large <- function(model, type_nodes, n_hat) {
  counts <- vapply(
    model$nodes[model$nodes %in% type_nodes],
    function(v) length(get_nodal_types(model, collapse = TRUE)[[v]]),
    numeric(1)
  )
  stop(
    "Factorized query (query_eval = \"ve\"): relevant type product is too large (",
    if (is.finite(n_hat)) format(round(n_hat), big.mark = ",") else "non-finite",
    "; product of nodal type counts on ",
    paste(names(counts), counts, sep = "=", collapse = " x "),
    "). ",
    "Restrict the model (simplify_model / set_restrictions) or raise ",
    "options(CausalQueries.factorized_ve_max) (default ",
    format(factorized_ve_max_types(), scientific = FALSE), "). ",
    "See ?query_model and memos/query_twin_network_ve.md.",
    call. = FALSE
  )
}

#' Build a causal_types data.frame for a slice of the relevant product.
#'
#' Indexing matches \code{expand.grid} over \code{model$nodes} with
#' placeholders of length 1 for omitted nodes (first column varies fastest).
#' @keywords internal
#' @noRd
causal_types_relevant_slice <- function(model, type_nodes, index_start, index_end) {
  nt <- get_nodal_types(model, collapse = TRUE)
  type_nodes <- model$nodes[model$nodes %in% type_nodes]
  n_slice <- as.integer(index_end - index_start + 1L)
  node_order <- model$nodes
  varying <- which(node_order %in% type_nodes)
  var_nodes <- node_order[varying]
  var_dims <- vapply(var_nodes, function(v) length(nt[[v]]), integer(1))

  out <- matrix(NA_character_, nrow = n_slice, ncol = length(node_order))
  colnames(out) <- node_order
  for (v in node_order) {
    if (!v %in% type_nodes) {
      out[, v] <- as.character(nt[[v]][1])
    }
  }
  pos0 <- as.integer(index_start:index_end) - 1L
  for (j in seq_along(var_nodes)) {
    v <- var_nodes[j]
    stride <- if (j == 1L) 1L else as.integer(prod(var_dims[seq_len(j - 1L)]))
    ix <- (pos0 %/% stride) %% var_dims[j] + 1L
    out[, v] <- as.character(nt[[v]])[ix]
  }
  df <- as.data.frame(out, stringsAsFactors = FALSE)
  cnames <- causal_type_names(df)
  rownames(df) <- do.call(paste, c(cnames, sep = "."))
  class(df) <- "data.frame"
  df
}

#' Chunked VE estimands: same kernels as grid, bounded memory, higher cap.
#' @keywords internal
#' @noRd
estimands_from_lambda_draws_ve <- function(model,
                                           type_nodes,
                                           query,
                                           given = TRUE,
                                           param_mat,
                                           join_by = "|",
                                           case_level = FALSE,
                                           using = "parameters") {
  n_hat <- estimate_relevant_type_product(model, type_nodes)
  ve_cap <- factorized_ve_max_types()
  if (!is.finite(n_hat) || n_hat > ve_cap) {
    stop_ve_product_too_large(model, type_nodes, n_hat)
  }

  n_types <- as.integer(round(n_hat))
  chunk <- factorized_ve_chunk_size()
  # Keep chunks small enough for nested-do realise caches; never one giant grid
  if (n_types <= chunk) {
    chunk <- n_types
  }
  n_draws <- nrow(param_mat)
  nums <- numeric(n_draws)
  dens <- numeric(n_draws)
  any_g <- FALSE

  start <- 1L
  while (start <= n_types) {
    end <- min(n_types, start + chunk - 1L)
    ct <- causal_types_relevant_slice(model, type_nodes, start, end)
    m <- model
    m$causal_types <- ct
    m$.cache <- new.env(parent = emptyenv())
    real <- realise_outcomes(m, add_rownames = TRUE)

    q_map <- map_query_to_causal_type(
      model = m, query = query, join_by = join_by, eval_var = real
    )
    x <- q_map$types

    if (isTRUE(given) || identical(as.character(given), "ALL") ||
        identical(as.character(given), "TRUE")) {
      g <- rep(TRUE, length(x))
    } else {
      g_map <- map_query_to_causal_type(
        model = m, query = as.character(given), join_by = join_by, eval_var = real
      )
      g <- g_map$types
      if (!is.logical(g)) {
        stop("`given` must evaluate to a logical condition on types.", call. = FALSE)
      }
    }

    if (any(g)) {
      any_g <- TRUE
      x_g <- as.numeric(x[g])
      W <- type_weights_matrix(model, param_mat, ct, type_nodes)
      Wg <- W[, g, drop = FALSE]
      dens <- dens + rowSums(Wg)
      nums <- nums + as.numeric(Wg %*% x_g)
    }

    start <- end + 1L
  }

  if (!any_g) {
    message("No units given. `NA` estimand.")
    return(rep(NA_real_, n_draws))
  }
  if (using != "parameters" && isTRUE(case_level)) {
    return(mean(nums) / mean(dens))
  }
  ifelse(dens > 0, nums / dens, NA_real_)
}
