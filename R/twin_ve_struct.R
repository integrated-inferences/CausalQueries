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

#' Lambda for one node given a partial type row (supports confound strata).
#' @keywords internal
#' @noRd
lambda_for_node_row <- function(model, param_row, node, type_row) {
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
  pn <- as.character(pdf$param_names[rows[idx]])
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
    gw <- tryCatch(parse_flat_constant_worlds(given), error = function(e) NULL)
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
#' @keywords internal
#' @noRd
realise_node_into_vals <- function(v, worlds, world_ids, parents, dos_val,
                                   tau, vals) {
  for (wid in world_ids) {
    if (!is.na(dos_val[[wid]])) {
      vals[[twin_val_name(v, wid)]] <- dos_val[[wid]]
      next
    }
    pa <- parents[[v]]
    if (!length(pa)) {
      vals[[twin_val_name(v, wid)]] <- as.integer(tau)
    } else {
      pv <- integer(length(pa))
      for (j in seq_along(pa)) {
        nm <- twin_val_name(pa[[j]], wid)
        if (!nm %in% names(vals)) {
          return(NULL)
        }
        pv[[j]] <- as.integer(vals[[nm]])
      }
      vals[[twin_val_name(v, wid)]] <- child_value_from_nodal_type(tau, pv)
    }
  }
  vals
}

#' Dos values for node v across worlds.
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
#' @keywords internal
#' @noRd
twin_struct_eliminate <- function(model, worlds, type_nodes, param_row,
                                  query = NULL, given = "ALL") {
  parents <- get_parents(model)
  nt <- get_nodal_types(model, collapse = TRUE)
  max_states <- ve_struct_max_states()
  type_nodes <- model$nodes[model$nodes %in% type_nodes]
  world_ids <- vapply(worlds, function(w) w$id, character(1))
  param_row <- clean_param_row(model, param_row)
  units <- struct_elim_units(model, type_nodes)
  done_nodes <- character(0)
  all_unit_nodes <- unlist(units, use.names = FALSE)

  state_w <- 1
  names(state_w) <- "__empty__"

  check_cap <- function(state_w, after) {
    if (length(state_w) > max_states) {
      stop(
        "Factorized query (query_eval = \"ve_struct\"): twin frontier exceeded ",
        "CausalQueries.ve_struct_max=", max_states, " (",
        length(state_w), " states after ", after, "). ",
        "Restrict the model or raise the option.",
        call. = FALSE
      )
    }
  }

  for (ui in seq_along(units)) {
    unit <- units[[ui]]
    new_keys <- character(0)
    new_w <- numeric(0)
    unit_nodes <- as.character(unit)
    remaining <- setdiff(all_unit_nodes, c(done_nodes, unit_nodes))

    # Joint type product for nodes in unit that enter type_nodes
    vary <- unit_nodes[unit_nodes %in% type_nodes]
    if (!length(vary)) {
      for (sk in names(state_w)) {
        base_w <- state_w[[sk]]
        vals <- parse_state_key(sk)
        ok <- TRUE
        for (v in unit_nodes) {
          dos_val <- dos_by_world(v, worlds, world_ids)
          tau <- as.character(nt[[v]][[1]])
          vals2 <- realise_node_into_vals(
            v, worlds, world_ids, parents, dos_val, tau, vals
          )
          if (is.null(vals2)) {
            ok <- FALSE
            break
          }
          vals <- vals2
        }
        if (ok) {
          new_keys <- c(new_keys, state_key(vals))
          new_w <- c(new_w, base_w)
        }
      }
    } else {
      type_lists <- lapply(vary, function(v) as.character(nt[[v]]))
      names(type_lists) <- vary
      grid <- do.call(expand.grid, c(type_lists, stringsAsFactors = FALSE))

      for (sk in names(state_w)) {
        base_w <- state_w[[sk]]
        base_vals <- parse_state_key(sk)
        for (gi in seq_len(nrow(grid))) {
          type_row <- as.list(grid[gi, , drop = FALSE])
          for (v in setdiff(unit_nodes, vary)) {
            type_row[[v]] <- as.character(nt[[v]][[1]])
          }
          lw <- 1
          ok_w <- TRUE
          for (v in vary) {
            lw <- lw * lambda_for_node_row(model, param_row, v, type_row)
            if (lw == 0) {
              ok_w <- FALSE
              break
            }
          }
          if (!ok_w || lw == 0) {
            next
          }
          vals <- base_vals
          ok <- TRUE
          for (v in unit_nodes) {
            dos_val <- dos_by_world(v, worlds, world_ids)
            tau <- as.character(type_row[[v]])
            vals2 <- realise_node_into_vals(
              v, worlds, world_ids, parents, dos_val, tau, vals
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
          new_keys <- c(new_keys, state_key(vals))
          new_w <- c(new_w, base_w * lw)
        }
      }
    }

    if (!length(new_keys)) {
      state_w <- numeric(0)
      names(state_w) <- character(0)
      break
    }
    agg <- aggregate_states(new_keys, new_w)
    state_w <- setNames(agg$weights, agg$keys)
    done_nodes <- c(done_nodes, unit_nodes)

    # Drop twin values that are no longer needed (frontier shrink)
    if (!is.null(query)) {
      keep <- twin_keep_value_names(
        model, worlds, world_ids, parents,
        done_nodes, remaining, query, given
      )
      # Always retain values just realized for nodes that are keep_nodes
      state_w <- project_states(state_w, keep)
    }
    check_cap(state_w, paste(unit_nodes, collapse = "+"))
  }

  state_w
}

#' Build one-row eval data for map_query-style payoff from twin values.
#'
#' Strategy: observational columns = node names from obs world (or first
#' world if none); each flat do atom replaced by writing outcome into a
#' scratch column and evaluating through map_query on a 1-row realise
#' built from a reconstructed type row is avoided — we substitute values
#' into the query string the same way map_query substitutes var_k.
#'
#' @keywords internal
#' @noRd
eval_fg_from_twin_state <- function(model, state_vals, worlds, query, given,
                                    join_by = "|") {
  # Reconstruct per-world node value lists
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

  # Data frame for eval_with_data: start with observational if present
  obs_id <- NULL
  for (w in worlds) {
    if (identical(w$label, "observational")) {
      obs_id <- w$id
      break
    }
  }
  if (is.null(obs_id)) {
    # synthetic observational from first world's non-dos realisation not needed;
    # use zeros placeholders — map_query only needs obs if stripped query has nodes
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
    # Walk worlds in the same reverse-bracket order as map_query / parse
    w_query <- unlist(strsplit(q0, ""))
    bracket_starts <- rev(grep("\\[", w_query))
    local_df <- df
    k <- ncol(local_df) + 1L
    for (i in seq_along(bracket_starts)) {
      .query <- w_query[bracket_starts[i]:length(w_query)]
      .bracket_ends <- grep("\\]", .query)[1]
      .query <- .query[1:.bracket_ends]
      inside <- paste0(.query[!grepl("\\[|\\]", .query)], collapse = "")
      parts <- if (!nzchar(inside)) character(0) else strsplit(inside, ",", fixed = TRUE)[[1]]
      dos <- list()
      for (p in parts) {
        sp <- strsplit(p, "=", fixed = TRUE)[[1]]
        dos[[sp[[1]]]] <- as.integer(sp[[2]])
      }
      b <- seq_len(bracket_starts[i])
      var <- paste0(w_query[b], collapse = "")
      var <- st_within(var)
      outcome <- var[length(var)]
      # Match world by dos + outcome
      wid <- NULL
      for (w in worlds) {
        if (identical(w$label, "observational")) {
          next
        }
        if (!identical(w$outcome, outcome)) {
          next
        }
        if (length(w$dos) != length(dos) ||
            !all(names(w$dos) %in% names(dos)) ||
            !all(vapply(names(w$dos), function(n) w$dos[[n]] == dos[[n]], logical(1)))) {
          next
        }
        wid <- w$id
        break
      }
      if (is.null(wid)) {
        stop("Could not match do-world for payoff.", call. = FALSE)
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

#' Estimands via structural twin VE for one admitted query.
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
  n_draws <- nrow(param_mat)
  nums <- numeric(n_draws)
  dens <- numeric(n_draws)
  any_g <- FALSE

  # Clean param mat colnames
  for (s in seq_len(n_draws)) {
    pr <- param_mat[s, ]
    if (is.null(names(pr)) || !all(model$parameters_df$param_names %in% names(pr))) {
      pr <- as.numeric(pr)
      names(pr) <- model$parameters_df$param_names
    }
    state_w <- twin_struct_eliminate(
      model, admit$worlds, type_nodes, pr,
      query = query, given = given
    )
    for (sk in names(state_w)) {
      wmass <- state_w[[sk]]
      vals <- parse_state_key(sk)
      fg <- eval_fg_from_twin_state(
        model, vals, admit$worlds, query, given, join_by = join_by
      )
      if (isTRUE(fg$g)) {
        any_g <- TRUE
        dens[s] <- dens[s] + wmass
        nums[s] <- nums[s] + wmass * fg$f
      }
    }
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
