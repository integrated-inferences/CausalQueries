#' Twin world specification + admission (M2)
#'
#' Flat do only. Nested / symbolic dos / confound-before-M6 → unsupported
#' with a structured reason (visible fallback).
#'
#' @keywords internal
#' @noRd
NULL

#' Detect nested do-brackets (e.g. B=B[A=0]).
#' @keywords internal
#' @noRd
query_has_nested_do <- function(query) {
  q <- gsub(" ", "", as.character(query))
  # Bracket opens inside another bracket, or a dos RHS itself contains '['
  grepl("\\[[^\\]]*\\[", q) || grepl("=[A-Za-z0-9_]*\\[", q)
}

#' Strip do-brackets (innermost-first) for observational node detection.
#' @keywords internal
#' @noRd
strip_do_brackets <- function(query) {
  q <- gsub(" ", "", check_query(as.character(query)))
  w <- unlist(strsplit(q, ""))
  repeat {
    starts <- grep("\\[", w)
    if (!length(starts)) {
      break
    }
    i <- starts[length(starts)]
    rest <- w[i:length(w)]
    end_rel <- grep("\\]", rest)[1]
    if (is.na(end_rel)) {
      break
    }
    end <- i + end_rel - 1L
    lhs <- st_within(paste0(w[seq_len(i)], collapse = ""))
    lhs <- lhs[length(lhs)]
    left <- i - nchar(lhs)
    if (left < 1L) {
      left <- i
    }
    w[left:end] <- ""
  }
  paste0(w, collapse = "")
}

#' Parse flat constant-dos worlds from a query; NULL if not flat.
#' Each world: list(outcome, dos = named integer 0/1, label).
#' @keywords internal
#' @noRd
parse_flat_constant_worlds <- function(query) {
  query <- gsub(" ", "", check_query(as.character(query)))
  if (query_has_nested_do(query)) {
    return(NULL)
  }
  w_query <- unlist(strsplit(query, ""))
  bracket_starts <- rev(grep("\\[", w_query))
  bracket_ends <- rev(grep("\\]", w_query))
  if (length(bracket_starts) != length(bracket_ends)) {
    return(NULL)
  }
  worlds <- list()
  if (!length(bracket_starts)) {
    return(worlds)
  }
  for (i in seq_along(bracket_starts)) {
    .query <- w_query[bracket_starts[i]:length(w_query)]
    .bracket_ends <- grep("\\]", .query)[1]
    .query <- .query[1:.bracket_ends]
    inside <- paste0(.query[!grepl("\\[|\\]", .query)], collapse = "")
    parts <- if (!nzchar(inside)) {
      character(0)
    } else {
      strsplit(inside, ",", fixed = TRUE)[[1]]
    }
    dos <- list()
    for (p in parts) {
      if (!nzchar(p)) {
        next
      }
      sp <- strsplit(p, "=", fixed = TRUE)[[1]]
      if (length(sp) != 2L || !sp[[2]] %in% c("0", "1")) {
        return(NULL)
      }
      if (!grepl("^[A-Za-z][A-Za-z0-9_]*$", sp[[1]])) {
        return(NULL)
      }
      dos[[sp[[1]]]] <- as.integer(sp[[2]])
    }
    b <- seq_len(bracket_starts[i])
    var <- paste0(w_query[b], collapse = "")
    var <- st_within(var)
    outcome <- var[length(var)]
    label <- paste0(
      outcome, "[",
      paste(sprintf("%s=%s", names(dos), unlist(dos)), collapse = ","),
      "]"
    )
    worlds[[length(worlds) + 1L]] <- list(
      outcome = outcome,
      dos = dos,
      label = label,
      id = paste0("w", length(worlds) + 1L)
    )
    var_length <- nchar(outcome)
    .end <- bracket_starts[i] + .bracket_ends - 1L
    s <- seq(bracket_starts[i] - var_length, .end)
    w_query[s[1]] <- paste0("var", length(worlds))
    if (length(s) > 1L) {
      w_query[s[2:length(s)]] <- ""
    }
  }
  worlds
}

#' Admit or reject structural twin VE for a query/given (M2).
#'
#' @return list(ok, reason, worlds, need_observational, query, given)
#' @keywords internal
#' @noRd
twin_world_admit <- function(model, query, given = "ALL",
                             confound_supported = FALSE) {
  query <- as.character(query)[[1]]
  g <- as.character(given)[[1]]
  need_obs <- FALSE

  if (isTRUE(model_has_confound(model)) && !isTRUE(confound_supported)) {
    return(list(
      ok = FALSE,
      reason = "confound_before_M6",
      worlds = NULL,
      need_observational = FALSE,
      query = query,
      given = g
    ))
  }

  q_worlds <- parse_flat_constant_worlds(query)
  if (is.null(q_worlds)) {
    reason <- if (query_has_nested_do(query)) "nested_do" else "non_constant_dos"
    return(list(
      ok = FALSE,
      reason = reason,
      worlds = NULL,
      need_observational = FALSE,
      query = query,
      given = g
    ))
  }

  stripped_q <- strip_do_brackets(query)
  obs_q <- nodes_in_statement(model$nodes, stripped_q)
  if (length(obs_q)) {
    need_obs <- TRUE
  }

  g_worlds <- list()
  if (!(isTRUE(given) || g %in% c("ALL", "TRUE"))) {
    if (query_has_nested_do(g)) {
      return(list(
        ok = FALSE,
        reason = "nested_do_given",
        worlds = NULL,
        need_observational = FALSE,
        query = query,
        given = g
      ))
    }
    g_worlds <- parse_flat_constant_worlds(g)
    if (is.null(g_worlds)) {
      return(list(
        ok = FALSE,
        reason = "non_constant_dos_given",
        worlds = NULL,
        need_observational = FALSE,
        query = query,
        given = g
      ))
    }
    stripped_g <- strip_do_brackets(g)
    obs_g <- nodes_in_statement(model$nodes, stripped_g)
    if (length(obs_g)) {
      need_obs <- TRUE
    }
  }

  # Dedupe worlds by label (same dos + outcome)
  worlds <- c(q_worlds, g_worlds)
  if (need_obs) {
    worlds[[length(worlds) + 1L]] <- list(
      outcome = NA_character_,
      dos = list(),
      label = "observational",
      id = "obs"
    )
  }
  if (length(worlds)) {
    labs <- vapply(worlds, function(w) w$label, character(1))
    worlds <- worlds[!duplicated(labs)]
    for (i in seq_along(worlds)) {
      worlds[[i]]$id <- if (identical(worlds[[i]]$label, "observational")) {
        "obs"
      } else {
        paste0("w", i)
      }
    }
  }

  list(
    ok = TRUE,
    reason = NA_character_,
    worlds = worlds,
    need_observational = need_obs,
    query = query,
    given = g
  )
}
