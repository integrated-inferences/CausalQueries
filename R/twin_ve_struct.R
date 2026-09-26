#' Structural twin-network VE (M3–M6)
#'
#' Topo-order twin frontier elimination with shared nodal types. Confound
#' components are atomic lambda blocks (same strata rules as
#' \code{type_weights_matrix} / \code{given_matches_type_row}).
#'
#' @keywords internal
#' @noRd
NULL

ve_struct_max_states <- function() {
  as.integer(getOption("CausalQueries.ve_struct_max", 1e6))
}

#' Draws processed per structural pass (priors / posteriors).
#' Larger values amortize combinatorial work; frontier x chunk uses memory.
#' @keywords internal
#' @noRd
ve_struct_draw_chunk <- function() {
  as.integer(getOption("CausalQueries.ve_struct_draw_chunk", 256L))
}

#' Normalize a one-row parameter vector to named param_names.
#' @keywords internal
#' @noRd
clean_param_row <- function(model, param_row) {
  pn <- model$parameters_df$param_names
  if (is.null(names(param_row)) || !all(pn %in% names(param_row))) {
    pr <- as.numeric(param_row)
    names(pr) <- pn
    return(pr)
  }
  pr <- as.numeric(param_row[pn])
  names(pr) <- pn
  pr
}

#' Align parameter draw matrix columns to model$parameters_df$param_names.
#' @keywords internal
#' @noRd
align_param_mat <- function(model, param_mat) {
  pn <- model$parameters_df$param_names
  if (is.null(colnames(param_mat)) || !all(pn %in% colnames(param_mat))) {
    if (ncol(param_mat) != length(pn)) {
      stop("param_mat columns do not match model parameters", call. = FALSE)
    }
    pm <- matrix(as.numeric(param_mat), nrow = nrow(param_mat), ncol = length(pn))
    colnames(pm) <- pn
    return(pm)
  }
  param_mat[, pn, drop = FALSE]
}

#' Column index in parameters_df / aligned param_mat for one (node, type_row).
#' @keywords internal
#' @noRd
lambda_param_col <- function(model, node, type_row) {
  pdf <- model$parameters_df
  rows <- which(pdf$node == node)
  if (!length(rows)) {
    stop("No parameters for node ", node, call. = FALSE)
  }
  labs <- as.character(pdf$nodal_type[rows])
  givens <- as.character(pdf$given[rows])
  ct_node <- as.character(type_row[[node]])
  has_strata <- any(nzchar(givens))
  if (!has_strata) {
    idx <- match(ct_node, labs)
    if (is.na(idx)) {
      stop("Missing lambda for ", node, " type ", ct_node, call. = FALSE)
    }
  } else {
    tr <- as.data.frame(type_row, stringsAsFactors = FALSE)
    hit <- which(
      labs == ct_node &
        vapply(givens, given_matches_type_row, logical(1), tr)
    )
    if (length(hit) != 1L) {
      stop(
        "Could not uniquely match stratified lambda for node ", node,
        call. = FALSE
      )
    }
    idx <- hit
  }
  # Absolute column among all parameters (aligned param_mat order)
  rows[idx]
}

#' Lambda for one node given a partial type row (supports confound strata).
#' @keywords internal
#' @noRd
lambda_for_node_row <- function(model, param_row, node, type_row) {
  col <- lambda_param_col(model, node, type_row)
  pn <- model$parameters_df$param_names[[col]]
  as.numeric(param_row[[pn]])
}

#' Unconfounded single-node lambda (no strata).
#' @keywords internal
#' @noRd
lambda_for_type <- function(model, param_row, node, nodal_type) {
  lambda_for_node_row(
    model, param_row, node,
    setNames(list(as.character(nodal_type)), node)
  )
}

#' Key for a named integer value map (sorted names).
#' @keywords internal
#' @noRd
state_key <- function(vals) {
  if (!length(vals)) {
    return("__empty__")
  }
  nm <- sort(names(vals))
  paste(nm, vapply(nm, function(n) as.character(vals[[n]]), character(1)),
        sep = "=", collapse = "|")
}

#' Parse value map from state key.
#' @keywords internal
#' @noRd
parse_state_key <- function(key) {
  if (!nzchar(key) || identical(key, "__empty__")) {
    return(list())
  }
  parts <- strsplit(key, "|", fixed = TRUE)[[1]]
  out <- list()
  for (p in parts) {
    sp <- strsplit(p, "=", fixed = TRUE)[[1]]
    out[[sp[[1]]]] <- as.integer(sp[[2]])
  }
  out
}

#' Aggregate weighted states (named list val_map -> weight).
#' @keywords internal
#' @noRd
aggregate_states <- function(keys, weights) {
  if (!length(keys)) {
    return(list(keys = character(0), weights = numeric(0), vals = list()))
  }
  u <- unique(keys)
  w <- numeric(length(u))
  names(w) <- u
  for (i in seq_along(keys)) {
    w[[keys[i]]] <- w[[keys[i]]] + weights[i]
  }
  list(keys = names(w), weights = as.numeric(w), vals = NULL)
}

#' Aggregate state weight *rows* (keys × draws) by state key.
#' @keywords internal
#' @noRd
aggregate_states_mat <- function(keys, W) {
  n_draws <- ncol(W)
  if (!length(keys)) {
    out <- matrix(0, nrow = 0, ncol = n_draws)
    return(out)
  }
  g <- factor(keys, levels = unique(keys))
  out <- rowsum(W, group = g, reorder = FALSE)
  rownames(out) <- levels(g)
  out
}

#' Twin value name: node in world id.
#' @keywords internal
#' @noRd
twin_val_name <- function(node, world_id) {
  paste(node, world_id, sep = "::")
}

#' Value names still needed after processing `done_nodes`.
#' @keywords internal
#' @noRd
twin_keep_value_names <- function(model, worlds, world_ids, parents,
                                  done_nodes, remaining_nodes, query, given) {
  keep_nodes <- character(0)
  # Parents of remaining nodes
  for (v in remaining_nodes) {
    keep_nodes <- union(keep_nodes, parents[[v]])
  }
  # Outcomes / observational nodes needed for payoff
  for (w in worlds) {
    if (!is.na(w$outcome) && nzchar(w$outcome)) {
      keep_nodes <- union(keep_nodes, w$outcome)
    }
  }
  if (!(isTRUE(given) || identical(as.character(given), "ALL") ||
        identical(as.character(given), "TRUE"))) {
    keep_nodes <- union(
      keep_nodes,
      observational_query_nodes(model, given)
    )
    # do-world outcomes in given
    gw <- tryCatch(parse_twin_worlds(given), error = function(e) NULL)
    if (!is.null(gw)) {
      for (w in gw) {
        if (!is.na(w$outcome) && nzchar(w$outcome)) {
          keep_nodes <- union(keep_nodes, w$outcome)
        }
      }
    }
  }
  keep_nodes <- union(
    keep_nodes,
    observational_query_nodes(model, query)
  )
  # Nested link sources (copy-from worlds)
  for (w in worlds) {
    if (!length(w$links)) {
      next
    }
    for (lk in w$links) {
      keep_nodes <- union(keep_nodes, lk$src_node)
    }
  }
  # Always keep remaining nodes' own values once realized? only if in keep
  keep_nodes <- intersect(model$nodes, keep_nodes)
  out <- character(0)
  for (v in keep_nodes) {
    for (wid in world_ids) {
      out <- c(out, twin_val_name(v, wid))
    }
  }
  unique(out)
}

#' Project and re-aggregate state weights onto keep names.
#' @keywords internal
#' @noRd
project_states <- function(state_w, keep_names) {
  if (!length(state_w)) {
    return(state_w)
  }
  new_keys <- character(length(state_w))
  new_w <- as.numeric(state_w)
  sks <- names(state_w)
  for (i in seq_along(sks)) {
    new_keys[i] <- project_state_key(sks[i], keep_names)
  }
  agg <- aggregate_states(new_keys, new_w)
  setNames(agg$weights, agg$keys)
}

#' Project state weight matrix onto keep names.
#' @keywords internal
#' @noRd
project_states_mat <- function(state_mat, keep_names) {
  if (!nrow(state_mat)) {
    return(state_mat)
  }
  sks <- rownames(state_mat)
  new_keys <- vapply(sks, project_state_key, character(1), keep_names,
                     USE.NAMES = FALSE)
  aggregate_states_mat(new_keys, state_mat)
}

#' Drop value vars from a state that are not in keep_names.
#' @keywords internal
#' @noRd
project_state_key <- function(key, keep_names) {
  vals <- parse_state_key(key)
  keep <- intersect(names(vals), keep_names)
  if (!length(keep)) {
    return("__empty__")
  }
  state_key(vals[keep])
}

#' Nodes whose types enter the structural product (same idea as query_type_nodes).
#' @keywords internal
#' @noRd
struct_type_nodes <- function(model, query, given) {
  query_type_nodes(model, query, given)
}

#' Elimination units: confound multi-node blocks are atomic.
#' @keywords internal
#' @noRd
struct_elim_units <- function(model, type_nodes) {
  type_nodes <- model$nodes[model$nodes %in% type_nodes]
  blocks <- factor_blocks_for_nodes(model, model$nodes)
  multi <- list()
  for (b in blocks) {
    b <- model$nodes[model$nodes %in% b]
    if (length(b) > 1L && any(b %in% type_nodes)) {
      multi[[length(multi) + 1L]] <- b
    }
  }
  done <- character(0)
  units <- list()
  for (v in model$nodes) {
    if (v %in% done) {
      next
    }
    hit <- NULL
    for (b in multi) {
      if (v %in% b) {
        hit <- b
        break
      }
    }
    if (!is.null(hit)) {
      units[[length(units) + 1L]] <- hit
      done <- c(done, hit)
    } else {
      units[[length(units) + 1L]] <- v
      done <- c(done, v)
    }
  }
  units
}

#' Realise one node in all worlds into vals; returns NULL if parents missing.
#' Supports constant dos and nested links (copy from another world).
#' @keywords internal
#' @noRd
realise_node_into_vals <- function(v, worlds, world_ids, parents, dos_spec,
                                   tau, vals) {
  # Multi-pass so link targets can wait for source values set in this call
  pending <- world_ids
  guard <- 0L
  while (length(pending) && guard <= length(world_ids) + 1L) {
    guard <- guard + 1L
    still <- character(0)
    for (wid in pending) {
      spec <- dos_spec[[wid]]
      nm <- twin_val_name(v, wid)
      if (identical(spec$kind, "const")) {
        vals[[nm]] <- as.integer(spec$value)
        next
      }
      if (identical(spec$kind, "link")) {
        src <- twin_val_name(spec$src_node, spec$src_world)
        if (!src %in% names(vals)) {
          still <- c(still, wid)
          next
        }
        vals[[nm]] <- as.integer(vals[[src]])
        next
      }
      # Natural realisation under this world's parents
      pa <- parents[[v]]
      if (!length(pa)) {
        vals[[nm]] <- as.integer(tau)
      } else {
        pv <- integer(length(pa))
        missing_pa <- FALSE
        for (j in seq_along(pa)) {
          pnm <- twin_val_name(pa[[j]], wid)
          if (!pnm %in% names(vals)) {
            missing_pa <- TRUE
            break
          }
          pv[[j]] <- as.integer(vals[[pnm]])
        }
        if (missing_pa) {
          return(NULL)
        }
        vals[[nm]] <- child_value_from_nodal_type(tau, pv)
      }
    }
    pending <- still
  }
  if (length(pending)) {
    return(NULL)
  }
  vals
}

#' Per-world do spec for node v: const, link, or natural.
#' @keywords internal
#' @noRd
dos_spec_by_world <- function(v, worlds, world_ids) {
  lab_to_id <- setNames(
    vapply(worlds, function(w) w$id, character(1)),
    vapply(worlds, function(w) w$label, character(1))
  )
  out <- vector("list", length(world_ids))
  names(out) <- world_ids
  for (w in worlds) {
    wid <- w$id
    if (v %in% names(w$dos)) {
      out[[wid]] <- list(kind = "const", value = as.integer(w$dos[[v]]))
    } else if (v %in% names(w$links)) {
      lk <- w$links[[v]]
      src_id <- unname(lab_to_id[[lk$src_label]])
      if (is.null(src_id) || is.na(src_id)) {
        stop("Missing link source world for label ", lk$src_label, call. = FALSE)
      }
      out[[wid]] <- list(
        kind = "link",
        src_world = src_id,
        src_node = lk$src_node
      )
    } else {
      out[[wid]] <- list(kind = "none")
    }
  }
  out
}

#' Dos values for node v across worlds (constants only; NA otherwise).
#' Kept for callers that only need constant interventions.
#' @keywords internal
#' @noRd
dos_by_world <- function(v, worlds, world_ids) {
  dos_val <- lapply(worlds, function(w) {
    if (v %in% names(w$dos)) as.integer(w$dos[[v]]) else NA_integer_
  })
  names(dos_val) <- world_ids
  dos_val
}

#' Structural elimination: weighted states over twin endogenous values.
#' Single parameter row; thin wrapper over the batched path.
#' @keywords internal
#' @noRd
twin_struct_eliminate <- function(model, worlds, type_nodes, param_row,
                                  query = NULL, given = "ALL") {
  param_row <- clean_param_row(model, param_row)
  pm <- matrix(as.numeric(param_row), nrow = 1L,
               dimnames = list(NULL, names(param_row)))
  sm <- twin_struct_eliminate_mat(
    model, worlds, type_nodes, pm, query = query, given = given
  )
  setNames(as.numeric(sm), rownames(sm))
}

#' Batched structural elimination: states x draws weight matrix.
#' Streams the frontier (no full edge list) so large DAGs stay in memory;
#' lambda products are vectorized over draw columns.
#' @keywords internal
#' @noRd
twin_struct_eliminate_mat <- function(model, worlds, type_nodes, param_mat,
                                      query = NULL, given = "ALL") {
  parents <- get_parents(model)
  nt <- get_nodal_types(model, collapse = TRUE)
  max_states <- ve_struct_max_states()
  type_nodes <- model$nodes[model$nodes %in% type_nodes]
  world_ids <- vapply(worlds, function(w) w$id, character(1))
  param_mat <- align_param_mat(model, param_mat)
  n_draws <- nrow(param_mat)
  units <- struct_elim_units(model, type_nodes)
  done_nodes <- character(0)
  all_unit_nodes <- unlist(units, use.names = FALSE)

  state_mat <- matrix(1, nrow = 1L, ncol = n_draws)
  rownames(state_mat) <- "__empty__"

  check_cap <- function(state_mat, after) {
    if (nrow(state_mat) > max_states) {
      stop(
        "Factorized query (query_eval = \"ve_struct\"): twin frontier exceeded ",
        "CausalQueries.ve_struct_max=", max_states, " (",
        nrow(state_mat), " states after ", after, "). ",
        "Restrict the model or raise the option.",
        call. = FALSE
      )
    }
  }

  flush_acc <- function(acc) {
    keys <- ls(envir = acc, all.names = TRUE)
    if (!length(keys)) {
      return(matrix(0, nrow = 0, ncol = n_draws))
    }
    out <- matrix(0, nrow = length(keys), ncol = n_draws)
    rownames(out) <- keys
    for (i in seq_along(keys)) {
      out[i, ] <- acc[[keys[[i]]]]
    }
    out
  }
  add_acc <- function(acc, key, w) {
    if (exists(key, envir = acc, inherits = FALSE)) {
      acc[[key]] <- acc[[key]] + w
    } else {
      acc[[key]] <- w
    }
  }

  for (ui in seq_along(units)) {
    unit <- units[[ui]]
    unit_nodes <- as.character(unit)
    remaining <- setdiff(all_unit_nodes, c(done_nodes, unit_nodes))
    old_keys <- rownames(state_mat)
    n_old <- length(old_keys)
    acc <- new.env(parent = emptyenv())

    vary <- unit_nodes[unit_nodes %in% type_nodes]
    if (!length(vary)) {
      for (si in seq_len(n_old)) {
        base_w <- state_mat[si, ]
        vals <- parse_state_key(old_keys[[si]])
        ok <- TRUE
        for (v in unit_nodes) {
          dos_spec <- dos_spec_by_world(v, worlds, world_ids)
          tau <- as.character(nt[[v]][[1]])
          vals2 <- realise_node_into_vals(
            v, worlds, world_ids, parents, dos_spec, tau, vals
          )
          if (is.null(vals2)) {
            ok <- FALSE
            break
          }
          vals <- vals2
        }
        if (ok) {
          add_acc(acc, state_key(vals), base_w)
        }
      }
    } else {
      type_lists <- lapply(vary, function(v) as.character(nt[[v]]))
      names(type_lists) <- vary
      grid <- do.call(expand.grid, c(type_lists, stringsAsFactors = FALSE))
      n_grid <- nrow(grid)

      grid_cols <- vector("list", n_grid)
      grid_types <- vector("list", n_grid)
      for (gi in seq_len(n_grid)) {
        type_row <- as.list(grid[gi, , drop = FALSE])
        for (v in setdiff(unit_nodes, vary)) {
          type_row[[v]] <- as.character(nt[[v]][[1]])
        }
        cols <- integer(length(vary))
        for (j in seq_along(vary)) {
          cols[[j]] <- lambda_param_col(model, vary[[j]], type_row)
        }
        grid_cols[[gi]] <- cols
        grid_types[[gi]] <- type_row
      }

      for (si in seq_len(n_old)) {
        base_w <- state_mat[si, ]
        base_vals <- parse_state_key(old_keys[[si]])
        for (gi in seq_len(n_grid)) {
          cols <- grid_cols[[gi]]
          lw <- base_w
          for (cj in cols) {
            lw <- lw * param_mat[, cj]
          }
          if (all(lw == 0)) {
            next
          }
          type_row <- grid_types[[gi]]
          vals <- base_vals
          ok <- TRUE
          for (v in unit_nodes) {
            dos_spec <- dos_spec_by_world(v, worlds, world_ids)
            tau <- as.character(type_row[[v]])
            vals2 <- realise_node_into_vals(
              v, worlds, world_ids, parents, dos_spec, tau, vals
            )
            if (is.null(vals2)) {
              ok <- FALSE
              break
            }
            vals <- vals2
          }
          if (!ok) {
            next
          }
          add_acc(acc, state_key(vals), lw)
        }
      }
    }

    state_mat <- flush_acc(acc)
    if (!nrow(state_mat)) {
      break
    }
    done_nodes <- c(done_nodes, unit_nodes)

    if (!is.null(query)) {
      keep <- twin_keep_value_names(
        model, worlds, world_ids, parents,
        done_nodes, remaining, query, given
      )
      state_mat <- project_states_mat(state_mat, keep)
    }
    check_cap(state_mat, paste(unit_nodes, collapse = "+"))
  }

  state_mat
}

#' Build one-row eval data for map_query-style payoff from twin values.
#' @keywords internal
#' @noRd
eval_fg_from_twin_state <- function(model, state_vals, worlds, query, given,
                                    join_by = "|") {
  world_vals <- list()
  for (w in worlds) {
    vals <- setNames(rep(NA_integer_, length(model$nodes)), model$nodes)
    for (v in model$nodes) {
      nm <- twin_val_name(v, w$id)
      if (nm %in% names(state_vals)) {
        vals[[v]] <- as.integer(state_vals[[nm]])
      }
    }
    world_vals[[w$id]] <- vals
  }

  obs_id <- NULL
  for (w in worlds) {
    if (identical(w$label, "observational")) {
      obs_id <- w$id
      break
    }
  }
  if (is.null(obs_id)) {
    df <- as.data.frame(
      lapply(model$nodes, function(v) 0L),
      stringsAsFactors = FALSE
    )
    names(df) <- model$nodes
  } else {
    df <- as.data.frame(
      lapply(model$nodes, function(v) as.integer(world_vals[[obs_id]][[v]])),
      stringsAsFactors = FALSE
    )
    names(df) <- model$nodes
  }

  substitute_flat_query <- function(q) {
    q0 <- gsub(" ", "", check_query(as.character(q)))
    if (!grepl("\\[", q0)) {
      return(list(expr = paste0("q <- ", q0), df = df))
    }
    # Innermost-first; world labels match parse_twin_worlds (incl. nested var_i)
    w_query <- unlist(strsplit(q0, ""))
    bracket_starts <- rev(grep("\\[", w_query))
    local_df <- df
    k <- ncol(local_df) + 1L
    lab_to_id <- setNames(
      vapply(worlds, function(w) w$id, character(1)),
      vapply(worlds, function(w) w$label, character(1))
    )
    for (i in seq_along(bracket_starts)) {
      .query <- w_query[bracket_starts[i]:length(w_query)]
      .bracket_ends <- grep("\\]", .query)[1]
      .query <- .query[1:.bracket_ends]
      inside <- paste0(.query[!grepl("\\[|\\]", .query)], collapse = "")
      b <- seq_len(bracket_starts[i])
      var <- paste0(w_query[b], collapse = "")
      var <- st_within(var)
      outcome <- var[length(var)]
      label <- paste0(outcome, "[", inside, "]")
      wid <- unname(lab_to_id[[label]])
      if (is.null(wid) || is.na(wid)) {
        stop("Could not match do-world for payoff: ", label, call. = FALSE)
      }
      local_df[[k]] <- as.integer(world_vals[[wid]][[outcome]])
      names(local_df)[k] <- paste0("var", i)
      var_length <- nchar(outcome)
      .end <- bracket_starts[i] + .bracket_ends - 1L
      s <- seq(bracket_starts[i] - var_length, .end)
      w_query[s[1]] <- paste0("var", i)
      if (length(s) > 1L) {
        w_query[s[2:length(s)]] <- ""
      }
      k <- k + 1L
    }
    expr <- paste0("q <- ", paste0(w_query, collapse = ""))
    list(expr = expr, df = local_df)
  }

  sq <- substitute_flat_query(query)
  f <- c(eval_with_data(sq$expr, sq$df))
  if (isTRUE(given) || identical(as.character(given), "ALL") ||
      identical(as.character(given), "TRUE")) {
    g <- TRUE
  } else {
    sg <- substitute_flat_query(given)
    g <- c(eval_with_data(sg$expr, sg$df))
    if (!is.logical(g)) {
      stop("`given` must evaluate to a logical condition on types.", call. = FALSE)
    }
  }
  list(f = as.numeric(f), g = as.logical(g))
}

#' Payoff f and given mask for each state key (parameter-independent).
#' @keywords internal
#' @noRd
eval_fg_for_state_keys <- function(model, state_keys, worlds, query, given,
                                   join_by = "|") {
  n <- length(state_keys)
  f <- numeric(n)
  g <- logical(n)
  for (i in seq_len(n)) {
    fg <- eval_fg_from_twin_state(
      model, parse_state_key(state_keys[[i]]), worlds, query, given,
      join_by = join_by
    )
    f[[i]] <- fg$f
    g[[i]] <- isTRUE(fg$g)
  }
  list(f = f, g = g)
}

#' Estimands via structural twin VE for one admitted query.
#' Batches prior/posterior draws; combinatorial work once per draw chunk.
#' Payoffs are cached by state key across chunks.
#' @keywords internal
#' @noRd
estimands_from_lambda_draws_ve_struct <- function(model,
                                                 admit,
                                                 query,
                                                 given = TRUE,
                                                 param_mat,
                                                 join_by = "|",
                                                 case_level = FALSE,
                                                 using = "parameters") {
  type_nodes <- struct_type_nodes(model, query, given)
  param_mat <- align_param_mat(model, param_mat)
  n_draws <- nrow(param_mat)
  nums <- numeric(n_draws)
  dens <- numeric(n_draws)
  any_g <- FALSE

  fg_f <- numeric(0)
  fg_g <- logical(0)

  chunk <- ve_struct_draw_chunk()
  if (!is.finite(chunk) || chunk < 1L) {
    chunk <- n_draws
  }
  chunk <- min(as.integer(chunk), n_draws)

  start <- 1L
  while (start <= n_draws) {
    end <- min(n_draws, start + chunk - 1L)
    pm <- param_mat[start:end, , drop = FALSE]
    state_mat <- twin_struct_eliminate_mat(
      model, admit$worlds, type_nodes, pm,
      query = query, given = given
    )
    if (nrow(state_mat)) {
      sk <- rownames(state_mat)
      known <- sk %in% names(fg_f)
      if (!all(known)) {
        new_sk <- sk[!known]
        fg2 <- eval_fg_for_state_keys(
          model, new_sk, admit$worlds, query, given, join_by = join_by
        )
        names(fg2$f) <- new_sk
        names(fg2$g) <- new_sk
        fg_f <- c(fg_f, fg2$f)
        fg_g <- c(fg_g, fg2$g)
      }
      f <- fg_f[sk]
      gmask <- as.logical(fg_g[sk])
      if (any(gmask)) {
        any_g <- TRUE
        Wg <- state_mat[gmask, , drop = FALSE]
        dens[start:end] <- dens[start:end] + colSums(Wg)
        nums[start:end] <- nums[start:end] +
          as.numeric(crossprod(f[gmask], Wg))
      }
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

#' Message for visible ve_struct fallback (lock 4).
#' @keywords internal
#' @noRd
message_ve_struct_fallback <- function(reason, fallback_method) {
  msg <- paste0(
    "query_eval = \"ve_struct\" not used (", reason, "); ",
    "falling back to query_eval = \"", fallback_method, "\". ",
    "See ?query_model (Factorized query_eval risks)."
  )
  message(msg)
  invisible(msg)
}
