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
