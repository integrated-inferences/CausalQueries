#' Data families / E matrix without causal types (factorized path).
#'
#' Same sign-matrix construction as \code{get_data_families}, but complete
#' data columns come from the 2^n grid rather than \code{realise_outcomes}.
#'
#' @keywords internal
#' @noRd
get_data_families_factorized <- function(model,
                                        drop_impossible = TRUE,
                                        drop_all_NA = TRUE,
                                        mapping_only = FALSE,
                                        sparse = NULL) {
  event <- NULL
  nodes <- model$nodes
  check_factorized_grid_size(model, "get_data_families_factorized")
  all_data <- get_all_data_types(model)
  full_data <- complete_data_grid(model)
  if (!nrow(full_data)) {
    stop(
      "get_data_families_factorized: no complete data patterns are consistent ",
      "with the model's nodal types.",
      call. = FALSE
    )
  }

  # sparse=TRUE: fill E by per-event completion (same answers; less peak RAM
  # on wide event lists). Default: dense multiply under the grid cap; sparse
  # when option CausalQueries.factorized_E_sparse is TRUE.
  if (is.null(sparse)) {
    sparse <- isTRUE(getOption("CausalQueries.factorized_E_sparse", FALSE))
  }

  if (sparse) {
    E <- matrix(0L, nrow = nrow(all_data), ncol = nrow(full_data),
                dimnames = list(all_data$event, full_data$event))
    full_mat <- as.matrix(full_data[nodes])
    for (i in seq_len(nrow(all_data))) {
      row <- all_data[i, nodes, drop = FALSE]
      obs <- nodes[!is.na(unlist(row))]
      if (!length(obs)) {
        E[i, ] <- 1L
        next
      }
      ok <- rep(TRUE, nrow(full_data))
      for (nm in obs) {
        ok <- ok & (as.integer(full_mat[, nm]) == as.integer(row[[nm]]))
      }
      E[i, ok] <- 1L
    }
  } else {
    sign_matrix <- (2 * as.matrix(all_data[nodes]) - 1)
    sign_matrix[is.na(sign_matrix)] <- 0
    type_matrix <- (2 * (as.matrix(full_data[nodes])) - 1)
    n_obs <- rowSums(abs(sign_matrix))
    E <- 1 * (sign_matrix %*% t(type_matrix) == n_obs)
    rownames(E) <- all_data$event
    colnames(E) <- full_data$event
  }

  keep <- rep(TRUE, nrow(E))
  if (drop_impossible) {
    keep[!(apply(E, 1, function(j) any(j == 1)))] <- FALSE
  }
  if (drop_all_NA) {
    keep[rownames(E) == "None"] <- FALSE
  }
  E <- E[keep, , drop = FALSE]
  all_data <- all_data[keep, ]
  possible_events <- rownames(E)

  which_strategy <- apply(all_data[nodes], 1, function(row) nodes[!is.na(row)])
  which_strategy <- which_strategy[lapply(which_strategy, length) != 0]

  if (!mapping_only) {
    E <- data.frame(
      event = possible_events,
      strategy = unlist(lapply(which_strategy, paste, collapse = "")),
      E,
      stringsAsFactors = FALSE
    )
    rownames(E) <- E$event
  }
  E
}

#' Prepare Stan data for the factorized model (no P / causal types).
#'
#' @keywords internal
#' @noRd
prep_stan_data_factorized <- function(model,
                                     data,
                                     keep_event_probabilities = FALSE,
                                     censored_types = NULL) {
  if (!all(c("event", "count") %in% names(data))) {
    stop("Data should contain columns `event` and `count`")
  }
  check_factorized_grid_size(model, "prep_stan_data_factorized")

  families <- get_data_families_factorized(model)
  data_families <- families[, setdiff(names(families), c("event", "strategy")),
                            drop = FALSE]
  n_data <- ncol(data_families)

  if (!("strategy" %in% names(data))) {
    names_check <- paste(data$event)
    data <- dplyr::left_join(families |> dplyr::select(event, strategy), data) |>
      dplyr::mutate(count = ifelse(is.na(count), 0L, as.integer(count)))
    if (!all(names_check %in% paste(data$event))) {
      stop(
        "Malformed event names provided in data.\n",
        "Generate compact data using collapse_data()"
      )
    }
    data <- data |>
      dplyr::group_by(strategy) |>
      dplyr::mutate(s = sum(count, na.rm = TRUE)) |>
      dplyr::filter(s > 0) |>
      dplyr::select(-s) |>
      dplyr::ungroup()
  }

  param_set <- model$parameters_df$param_set
  param_sets <- unique(param_set)
  n_param_sets <- length(param_sets)
  n_param_each <- vapply(param_sets, function(j) sum(param_set == j), numeric(1))
  l_ends <- as.array(cumsum(n_param_each))
  l_starts <- if (length(l_ends) == 1) {
    1
  } else {
    c(1, l_ends[1:(n_param_sets - 1)] + 1)
  }
  names(l_starts) <- names(l_ends)

  nodes_sets <- model$parameters_df |>
    dplyr::mutate(i = seq_len(dplyr::n())) |>
    dplyr::group_by(node) |>
    dplyr::summarize(n_starts = i[1], n_ends = i[dplyr::n()], .groups = "drop") |>
    dplyr::arrange(n_starts)
  n_starts <- nodes_sets$n_starts
  n_ends <- nodes_sets$n_ends
  n_sets <- nodes_sets$node
  names(n_starts) <- names(n_ends) <- n_sets

  if (!is.null(censored_types)) {
    unknown <- setdiff(as.character(censored_types), rownames(data_families))
    if (length(unknown) > 0) {
      stop("Unrecognized `censored_types`: ", paste(unknown, collapse = ", "))
    }
  }
  data <- data[!c(data$event %in% censored_types), ]

  E <- data_families[data$event, ] |> as.matrix()
  strategies <- data$strategy
  n_strategies <- length(unique(strategies))
  w_starts <- which(!duplicated(strategies))
  k <- length(strategies)
  w_ends <- if (n_strategies < 2) {
    k
  } else {
    c(w_starts[2:n_strategies] - 1, k)
  }

  parmap <- make_parmap_factorized(model)
  map <- attr(parmap, "map")

  list(
    parmap = parmap,
    map = map,
    n_paths = nrow(map),
    n_params = nrow(parmap),
    n_param_sets = n_param_sets,
    n_param_each = as.array(n_param_each),
    l_starts = as.array(l_starts),
    l_ends = as.array(l_ends),
    node_starts = as.array(n_starts),
    node_ends = as.array(n_ends),
    n_nodes = length(n_sets),
    lambdas_prior = get_priors(model),
    n_data = n_data,
    n_events = nrow(E),
    n_strategies = n_strategies,
    strategy_starts = as.array(w_starts),
    strategy_ends = as.array(w_ends),
    E = E,
    Y = as.array(data$count),
    data = data
  )
}

#' Return the precompiled factorized Stan model.
#'
#' @keywords internal
#' @noRd
get_stanmodel_factorized <- function() {
  if (exists("stanmodels", inherits = TRUE)) {
    sm <- get("stanmodels", inherits = TRUE)
    if (!is.null(sm$simplexes_factorized)) {
      return(sm$simplexes_factorized)
    }
  }
  stop(
    "Precompiled Stan model `simplexes_factorized` is not available. ",
    "Reinstall CausalQueries from source (e.g. ",
    "`R CMD INSTALL --preclean CausalQueries`) so both Stan models are built. ",
    "Do not rely on on-the-fly compilation.",
    call. = FALSE
  )
}
