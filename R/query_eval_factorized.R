#' Parameters-only query evaluation (factorized path)
#'
#' Relevant-set evaluation over nodal types: expand only types that enter the
#' query, weight by lambda products (stratified under confound) without storing
#' type_posterior. Confound merges factor_blocks.
#'
#' Factorized evaluators (\code{query_eval}, not a second \code{legacy}):
#' \describe{
#'   \item{\code{"grid"} (default)}{Relevant-set \code{expand.grid} then
#'     \code{realise_outcomes} / \code{map_query_to_causal_type}.}
#'   \item{\code{"ve"}}{Chunked twin-network accumulation with the same kernels;
#'     allows a higher product cap for large queries.}
#'   \item{\code{"auto"}}{\code{"grid"} when the product fits; \code{"ve"} when
#'     the grid path would refuse.}
#' }
#' See \code{memos/query_twin_network_ve.md} and \code{?query_model}.
#'
#' @keywords internal
#' @noRd

check_factorized_query <- function(model, what = "query") {
  invisible(model)
}

#' Ancestors of nodes (including the nodes themselves), in model node order.
#' @keywords internal
#' @noRd
get_ancestors <- function(model, nodes) {
  nodes <- unique(as.character(nodes))
  nodes <- nodes[nodes %in% model$nodes]
  if (length(nodes) == 0L) {
    return(character(0))
  }
  parents <- get_parents(model)
  frontier <- nodes
  seen <- nodes
  while (length(frontier)) {
    add <- unique(unlist(parents[frontier], use.names = FALSE))
    add <- setdiff(add, seen)
    if (!length(add)) {
      break
    }
    seen <- c(seen, add)
    frontier <- add
  }
  model$nodes[model$nodes %in% seen]
}

#' Factor blocks on type_nodes: merge confound-connected components.
#' @keywords internal
#' @noRd
factor_blocks_for_nodes <- function(model, type_nodes) {
  type_nodes <- model$nodes[model$nodes %in% type_nodes]
  if (!length(type_nodes)) {
    return(list())
  }
  comps <- confound_components(model)
  blocks <- list()
  used <- character(0)
  for (comp in comps) {
    hit <- intersect(comp, type_nodes)
    if (length(hit)) {
      blocks[[length(blocks) + 1L]] <- hit
      used <- c(used, hit)
    }
  }
  for (v in setdiff(type_nodes, used)) {
    blocks[[length(blocks) + 1L]] <- v
  }
  blocks
}

#' Whether parameters_df$given matches conditioner types in a causal-type row.
#' @keywords internal
#' @noRd
given_matches_type_row <- function(given_str, type_row) {
  given_str <- as.character(given_str)
  if (!nzchar(given_str)) {
    return(TRUE)
  }
  parts <- strsplit(given_str, ", ", fixed = TRUE)[[1]]
  for (p in parts) {
    sp <- strsplit(p, ".", fixed = TRUE)[[1]]
    if (length(sp) < 2L) {
      return(FALSE)
    }
    node <- sp[[1]]
    typ <- paste(sp[-1], collapse = ".")
    if (!node %in% names(type_row)) {
      return(FALSE)
    }
    if (!identical(as.character(type_row[[node]]), typ)) {
      return(FALSE)
    }
  }
  TRUE
}

#' Expand type_nodes to include full confound blocks.
#' @keywords internal
#' @noRd
expand_type_nodes_confound <- function(model, type_nodes) {
  if (!length(type_nodes) || !model_has_confound(model)) {
    return(type_nodes)
  }
  comps <- confound_components(model)
  out <- type_nodes
  for (comp in comps) {
    if (length(intersect(comp, type_nodes))) {
      out <- union(out, comp)
    }
  }
  model$nodes[model$nodes %in% out]
}

#' Parse do-worlds in a query (innermost brackets first), as in map_query.
#' Each world: outcome node + syntactic dos targets.
#' @keywords internal
#' @noRd
parse_query_do_worlds <- function(query) {
  query <- gsub(" ", "", check_query(as.character(query)))
  w_query <- unlist(strsplit(query, ""))
  bracket_starts <- rev(grep("\\[", w_query))
  bracket_ends <- rev(grep("\\]", w_query))
  if (length(bracket_starts) != length(bracket_ends)) {
    stop("Either '[' or ']' missing in query.", call. = FALSE)
  }
  worlds <- list()
  if (!length(bracket_starts)) {
    return(worlds)
  }

  for (i in seq_along(bracket_starts)) {
    .query <- w_query[bracket_starts[i]:length(w_query)]
    .bracket_ends <- grep("\\]", .query)[1]
    .query <- .query[1:.bracket_ends]
    .query <- .query[!grepl("\\[|\\]", .query)]
    .query <- paste0(.query, collapse = "")
    parts <- unlist(strsplit(.query, ","))
    dos_targets <- character(0)
    for (part in parts) {
      if (!nzchar(part)) {
        next
      }
      eq <- gregexpr("=", part, perl = TRUE)[[1]][1]
      if (eq < 1) {
        next
      }
      var_name <- gsub(" ", "", substr(part, 1, eq - 1))
      dos_targets <- c(dos_targets, var_name)
    }

    b <- seq_len(bracket_starts[i])
    var <- paste0(w_query[b], collapse = "")
    var <- st_within(var)
    outcome <- var[length(var)]

    worlds[[length(worlds) + 1L]] <- list(
      outcome = outcome,
      dos = unique(dos_targets)
    )

    # Same substitution as map_query so outer brackets see cleaned text
    var_length <- nchar(outcome)
    .end <- bracket_starts[i] + .bracket_ends - 1
    s <- seq(bracket_starts[i] - var_length, .end)
    w_query[s[1]] <- paste0("var", i)
    if (length(s) > 1) {
      w_query[s[2:length(s)]] <- ""
    }
  }
  worlds
}

#' Nodes that appear observationally (outside do-brackets).
#' @keywords internal
#' @noRd
observational_query_nodes <- function(model, query) {
  q <- gsub(" ", "", check_query(as.character(query)))
  # Drop Node[...] chunks innermost-first
  w <- unlist(strsplit(q, ""))
  repeat {
    starts <- grep("\\[", w)
    if (!length(starts)) {
      break
    }
    i <- starts[length(starts)]
    rest <- w[i:length(w)]
    end_rel <- grep("\\]", rest)[1]
    end <- i + end_rel - 1
    # include LHS node immediately before '['
    lhs <- st_within(paste0(w[seq_len(i)], collapse = ""))
    lhs <- lhs[length(lhs)]
    left <- i - nchar(lhs)
    w[left:end] <- ""
  }
  stripped <- paste0(w, collapse = "")
  nodes_in_statement(model$nodes, stripped)
}

#' Nodal types that enter the unconfounded weight product for a query/given.
#' @keywords internal
#' @noRd
query_type_nodes <- function(model, query, given = "ALL") {
  needed <- character(0)

  add_worlds <- function(q) {
    worlds <- parse_query_do_worlds(q)
    for (w in worlds) {
      anc <- get_ancestors(model, w$outcome)
      needed <<- union(needed, setdiff(anc, w$dos))
    }
    obs <- observational_query_nodes(model, q)
    if (length(obs)) {
      needed <<- union(needed, get_ancestors(model, obs))
    }
  }

  add_worlds(query)
  g <- as.character(given)
  if (!(isTRUE(given) || g %in% c("ALL", "TRUE"))) {
    add_worlds(g)
  }

  if (!length(needed)) {
    # Degenerate: fall back to all nodes
    return(model$nodes)
  }
  expand_type_nodes_confound(model, model$nodes[model$nodes %in% needed])
}

#' Causal-type grid over type_nodes only; other nodes fixed to a placeholder type.
#' @keywords internal
#' @noRd
causal_types_relevant <- function(model, type_nodes) {
  nt <- get_nodal_types(model, collapse = TRUE)
  type_nodes <- model$nodes[model$nodes %in% type_nodes]
  if (!length(type_nodes)) {
    stop("No type nodes for factorized query.", call. = FALSE)
  }

  grid_list <- lapply(model$nodes, function(v) {
    if (v %in% type_nodes) {
      as.character(nt[[v]])
    } else {
      # Placeholder: never read when v is always intervened / summed out
      as.character(nt[[v]][1])
    }
  })
  names(grid_list) <- model$nodes
  df <- expand.grid(grid_list, stringsAsFactors = FALSE)
  cnames <- causal_type_names(df)
  rownames(df) <- do.call(paste, c(cnames, sep = "."))
  class(df) <- "data.frame"
  df
}

#' Vectorized type weights: draws × types (product of lambdas on type_nodes).
#' Respects confound strata via parameters_df$given.
#' @keywords internal
#' @noRd
type_weights_matrix <- function(model, param_mat, causal_types, type_nodes) {
  parameters <- clean_param_vector(model, param_mat[1, ])
  pn <- names(parameters)
  if (is.null(colnames(param_mat)) || !all(pn %in% colnames(param_mat))) {
    pm <- matrix(as.numeric(param_mat), nrow = nrow(param_mat))
    colnames(pm) <- model$parameters_df$param_names
  } else {
    pm <- param_mat[, pn, drop = FALSE]
  }

  pdf <- model$parameters_df
  n_draws <- nrow(pm)
  n_types <- nrow(causal_types)
  W <- matrix(1, nrow = n_draws, ncol = n_types)

  blocks <- factor_blocks_for_nodes(model, type_nodes)
  for (block in blocks) {
    for (node in block) {
      rows <- which(pdf$node == node)
      labs <- as.character(pdf$nodal_type[rows])
      givens <- as.character(pdf$given[rows])
      has_strata <- any(nzchar(givens))

      if (!has_strata) {
        idx <- match(as.character(causal_types[[node]]), labs)
      } else {
        idx <- integer(n_types)
        ct_node <- as.character(causal_types[[node]])
        for (j in seq_len(n_types)) {
          type_row <- causal_types[j, , drop = FALSE]
          hit <- which(
            labs == ct_node[j] &
              vapply(givens, given_matches_type_row, logical(1), type_row)
          )
          if (length(hit) != 1L) {
            stop(
              "Could not uniquely match stratified lambda for node ", node,
              " on type row ", j, call. = FALSE
            )
          }
          idx[j] <- hit
        }
      }

      if (anyNA(idx)) {
        stop("Missing lambda for a nodal type on node ", node, call. = FALSE)
      }
      W <- W * pm[, rows[idx], drop = FALSE]
    }
  }
  W
}

#' Product of nodal-type counts on type_nodes (log-space; Inf if huge).
#' @keywords internal
#' @noRd
estimate_relevant_type_product <- function(model, type_nodes) {
  nt <- get_nodal_types(model, collapse = TRUE)
  type_nodes <- model$nodes[model$nodes %in% type_nodes]
  if (!length(type_nodes)) {
    return(0)
  }
  counts <- vapply(type_nodes, function(v) length(nt[[v]]), numeric(1))
  if (any(!is.finite(counts)) || any(counts < 1)) {
    return(Inf)
  }
  log_n <- sum(log(counts))
  if (!is.finite(log_n) || log_n > log(.Machine$double.xmax)) {
    return(Inf)
  }
  exp(log_n)
}

#' One-shot VE schedule: relevant types + realisations (not kept on model).
#' @keywords internal
#' @noRd
factorized_query_schedule <- function(model, query = NULL, given = "ALL") {
  check_factorized_query(model, "query")

  if (is.null(query)) {
    type_nodes <- model$nodes
  } else {
    type_nodes <- query_type_nodes(model, query, given)
  }

  # Refuse before expand.grid — same threshold, no change to answered queries
  n_hat <- estimate_relevant_type_product(model, type_nodes)
  max_types <- factorized_query_max_types()
  if (!is.finite(n_hat) || n_hat > max_types) {
    counts <- vapply(
      model$nodes[model$nodes %in% type_nodes],
      function(v) length(get_nodal_types(model, collapse = TRUE)[[v]]),
      numeric(1)
    )
    stop(
      "Factorized query (query_eval = \"grid\"): relevant type product is too large (",
      if (is.finite(n_hat)) format(round(n_hat), big.mark = ",") else "non-finite",
      "; product of nodal type counts on ",
      paste(names(counts), counts, sep = "=", collapse = " x "),
      "). ",
      "Try query_eval = \"ve\" or \"auto\" for chunked evaluation, ",
      "legacy = TRUE, or restrict the model. See ?query_model.",
      call. = FALSE
    )
  }

  ct <- causal_types_relevant(model, type_nodes)
  n_types <- nrow(ct)
  if (!is.finite(n_types) || n_types > max_types) {
    stop(
      "Factorized query (query_eval = \"grid\"): relevant type product is too large (",
      format(n_types, big.mark = ","), "). ",
      "Try query_eval = \"ve\" or \"auto\", legacy = TRUE, or restrict the model. ",
      "See ?query_model.",
      call. = FALSE
    )
  }

  # Shallow copy: private .cache so relevant-set realisations do not share
  # keys with the caller's model cache (even with types_fp in the key).
  m <- model
  m$causal_types <- ct
  m$.cache <- new.env(parent = emptyenv())
  list(
    causal_types = ct,
    type_nodes = type_nodes,
    realisations = realise_outcomes(m, add_rownames = TRUE),
    model = m
  )
}

#' Parameter draw matrix for using= parameters / priors / posteriors.
#' @keywords internal
#' @noRd
factorized_param_draws <- function(model, using, parameters = NULL, n_draws = 4000) {
  using <- using[[1]]
  if (using == "parameters") {
    if (is.null(parameters)) {
      parameters <- get_parameters(model)
    }
    parameters <- clean_param_vector(model, parameters)
    return(matrix(as.numeric(parameters), nrow = 1,
                  dimnames = list(NULL, names(parameters))))
  }
  if (using == "priors") {
    if (is.null(model$prior_distribution)) {
      message("Prior distribution added to model")
      model <- set_prior_distribution(model, n_draws = n_draws)
    }
    return(as.matrix(model$prior_distribution))
  }
  if (using == "posteriors") {
    if (!has_posterior(model)) {
      stop("Model does not contain a posterior distribution", call. = FALSE)
    }
    return(as.matrix(model$posterior_distribution))
  }
  stop("using must be parameters, priors, or posteriors", call. = FALSE)
}

#' Estimand draws via relevant-set VE (no type_posterior).
#' @keywords internal
#' @noRd
estimands_from_lambda_draws <- function(model,
                                        schedule,
                                        query,
                                        given = TRUE,
                                        param_mat,
                                        join_by = "|",
                                        case_level = FALSE,
                                        using = "parameters") {
  m <- schedule$model
  real <- schedule$realisations
  ct <- schedule$causal_types
  type_nodes <- schedule$type_nodes

  q_map <- map_query_to_causal_type(
    model = m,
    query = query,
    join_by = join_by,
    eval_var = real
  )
  x <- q_map$types

  if (isTRUE(given) || identical(as.character(given), "ALL") ||
      identical(as.character(given), "TRUE")) {
    g <- rep(TRUE, length(x))
  } else {
    g_map <- map_query_to_causal_type(
      model = m,
      query = as.character(given),
      join_by = join_by,
      eval_var = real
    )
    g <- g_map$types
    if (!is.logical(g)) {
      stop("`given` must evaluate to a logical condition on types.", call. = FALSE)
    }
  }

  if (!any(g)) {
    message("No units given. `NA` estimand.")
    return(rep(NA_real_, nrow(param_mat)))
  }

  x_g <- as.numeric(x[g])
  W <- type_weights_matrix(model, param_mat, ct, type_nodes)
  Wg <- W[, g, drop = FALSE]
  dens <- rowSums(Wg)
  nums <- as.numeric(Wg %*% x_g)

  if (using != "parameters" && isTRUE(case_level)) {
    return(mean(nums) / mean(dens))
  }
  ifelse(dens > 0, nums / dens, NA_real_)
}

#' Factorized query_distribution for a single model.
#' @keywords internal
#' @noRd
query_distribution_factorized <- function(model,
                                         queries,
                                         given,
                                         using,
                                         parameters = NULL,
                                         n_draws = 4000,
                                         join_by = "|",
                                         case_level = FALSE,
                                         query_eval = NULL) {
  check_factorized_query(model, "query_distribution")
  query_eval <- resolve_query_eval(query_eval)

  using_v <- unlist(using)
  given_v <- unlist(given)
  case_v <- unlist(case_level)
  if (length(case_v) == 1L) {
    case_v <- rep(case_v, length(queries))
  }
  if (length(using_v) == 1L) {
    using_v <- rep(using_v, length(queries))
  }
  if (length(given_v) == 1L) {
    given_v <- rep(given_v, length(queries))
  }

  q_chr <- vapply(queries, as.character, character(1))
  g_chr <- vapply(given_v, as.character, character(1))
  for (i in seq_along(q_chr)) {
    if (grepl(":\\|:", q_chr[i])) {
      sp <- deparse_given(q_chr[i])
      q_chr[i] <- sp$query
      g_chr[i] <- sp$given
    }
  }

  param_vec <- NULL
  if (!is.null(parameters)) {
    param_vec <- if (is.list(parameters)) parameters[[1]] else parameters
  }

  cols <- vector("list", length(q_chr))
  for (i in seq_along(q_chr)) {
    choice <- choose_factorized_query_eval(
      model, q_chr[i], g_chr[i], query_eval
    )
    pm <- factorized_param_draws(
      model, using_v[i],
      parameters = param_vec,
      n_draws = n_draws
    )
    cols[[i]] <- estimands_factorized_dispatch(
      model = model,
      query = q_chr[i],
      given = g_chr[i],
      param_mat = pm,
      method = choice$method,
      type_nodes = choice$type_nodes,
      join_by = join_by,
      case_level = case_v[i],
      using = using_v[i]
    )
  }

  lens <- vapply(cols, length, integer(1))
  if (any(lens == 1L) && max(lens) > 1L) {
    for (i in which(lens == 1L)) {
      cols[[i]] <- rep(cols[[i]], max(lens))
    }
  }

  out <- as.data.frame(cols, optional = TRUE)
  if (!is.null(names(queries))) {
    colnames(out) <- make.unique(names(queries), sep = "_")
  } else {
    # Match legacy query_distribution naming: append " :|: <given>"
    given_names <- vapply(g_chr, function(g) {
      if (g %in% c("ALL", "TRUE") || identical(g, "TRUE")) {
        ""
      } else {
        paste0(" :|: ", g)
      }
    }, character(1))
    colnames(out) <- make.unique(paste0(q_chr, given_names), sep = "_")
  }
  out
}
