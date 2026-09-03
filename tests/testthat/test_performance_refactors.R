context("Equivalence tests for safe speed refactors")

test_that("get_estimands colSums path matches apply-based formula", {
  model <- make_model("X -> Y") |>
    set_prior_distribution(n_draws = 25)

  query <- "Y[X=1] - Y[X=0]"
  given_q <- "X==1 & Y==1"
  realised <- realise_outcomes(model)
  x <- CausalQueries:::map_query_to_causal_type(
    model, query, eval_var = realised)$types
  given <- CausalQueries:::map_query_to_causal_type(
    model, given_q, eval_var = realised)$types
  tp <- CausalQueries:::get_type_prob_multiple(model, using = "priors")

  x_g <- x[given]
  tp_g <- tp[given, , drop = FALSE]
  pop_old <- as.vector((x_g %*% tp_g) / apply(tp_g, 2, sum))
  case_old <- mean(x_g %*% tp_g) / mean(apply(tp_g, 2, sum))

  q_pop <- query_model(model, query, given = given_q, using = "priors",
                       stats = c(mean = mean))
  expect_equal(q_pop$mean, mean(pop_old))

  q_uncond <- query_model(model, query, using = "priors",
                          stats = c(mean = mean))
  pop_uncond <- as.vector((x %*% tp) / apply(tp, 2, sum))
  expect_equal(q_uncond$mean, mean(pop_uncond))

  q_case <- query_model(model, query, given = given_q, using = "priors",
                        case_level = TRUE, stats = c(mean = mean))
  expect_equal(q_case$mean, case_old)
})


test_that("set_confound rowSums filter drops all-zero rows and keeps P aligned", {
  model <- make_model("X -> Y") |> set_confound(list("X <-> Y"))

  expect_equal(nrow(model$P), nrow(model$parameters_df))
  expect_equal(rownames(model$P), model$parameters_df$param_names)
  expect_true(all(rowSums(as.matrix(model$P)) != 0))
  expect_equal(ncol(model$P), 8L)

  model2 <- make_model("X -> Y -> W") |>
    set_confound(list("Y <-> X", "X <-> W"))
  expect_equal(nrow(model2$P), nrow(model2$parameters_df))
  expect_equal(rownames(model2$P), model2$parameters_df$param_names)
  expect_true(all(rowSums(as.matrix(model2$P)) != 0))
})


test_that("set_restrictions keep_ct keeps the same causal-type columns", {
  unrestricted <- make_model("X -> Y")
  P0 <- CausalQueries:::get_parameter_matrix(unrestricted)
  restricted <- set_restrictions(unrestricted, labels = list(Y = "00"))

  dropped <- setdiff(colnames(P0), colnames(restricted$P))
  expect_equal(ncol(restricted$P), 6L)
  expect_true(all(grepl("Y00", dropped)))
  expect_false(any(grepl("Y00", colnames(restricted$P))))
  expect_false("Y.00" %in% restricted$parameters_df$param_names)
  expect_equal(rownames(restricted$P), restricted$parameters_df$param_names)

  m <- make_model("X -> Y <- Z")
  P0 <- CausalQueries:::get_parameter_matrix(m)
  mr <- set_restrictions(m, statement = c("(X[] == 1)", "(Z[] == 1)"))
  expect_equal(ncol(P0), 64L)
  expect_equal(ncol(mr$P), 16L)
  expect_equal(sum(mr$parameters_df$node == "X"), 1L)
  expect_equal(sum(mr$parameters_df$node == "Z"), 1L)
  expect_false("X.1" %in% mr$parameters_df$param_names)
  expect_false("Z.1" %in% mr$parameters_df$param_names)
  expect_equal(rownames(mr$P), mr$parameters_df$param_names)
})


test_that("get_event_probabilities matches the previous node-sum/product formula", {
  old_event_probs <- function(model, parameters = NULL) {
    if (!is.null(parameters)) {
      parameters <- CausalQueries:::clean_param_vector(model, parameters)
    } else {
      parameters <- CausalQueries:::get_parameters(model)
    }
    parmap <- CausalQueries:::get_parmap(model)
    map <- t(attr(parmap, "map"))
    x <- (parmap * parameters) |>
      data.frame() |>
      dplyr::group_by(model$parameters_df$node) |>
      dplyr::summarize_all(sum) |>
      dplyr::select(-1) |>
      dplyr::summarize_all(prod) |>
      t()
    map %*% x
  }

  model <- make_model("X -> Y")
  w <- get_event_probabilities(model)
  expect_equal(rownames(w), c("X0Y0", "X1Y0", "X0Y1", "X1Y1"))
  expect_equal(as.vector(w), as.vector(old_event_probs(model)))
  expect_equal(sum(w), 1, tolerance = 1e-12)

  w_par <- get_event_probabilities(model, parameters = 1:6)
  expect_equal(rownames(w_par), rownames(w))
  expect_equal(as.vector(w_par), as.vector(old_event_probs(model, 1:6)))

  mr <- set_restrictions(make_model("X -> Y"), labels = list(Y = "00"),
                         keep = TRUE)
  w_r <- get_event_probabilities(mr)
  expect_equal(rownames(w_r), colnames(get_ambiguities_matrix(mr)))
  expect_equal(as.vector(w_r), as.vector(old_event_probs(mr)))

  # given= path: unrestricted values unchanged; restricted conditioned
  # on possible events (item 3.9)
  p_given <- as.vector(get_event_probabilities(model, given = "X!=Y"))
  expect_equal(p_given, c(0, 0.5, 0.5, 0))

  w_r_g <- get_event_probabilities(mr, given = "X==1")
  expect_equal(rownames(w_r_g), rownames(w_r))
  expect_equal(sum(w_r_g), 1, tolerance = 1e-12)
  expect_equal(as.vector(w_r_g)[rownames(w_r_g) == "X0Y0"], 0)
  expect_equal(as.vector(w_r_g)[rownames(w_r_g) == "X1Y0"], 1)
})


test_that("prep_stan_data hoist preserves n_data, E, map, and strategy handling", {
  model <- make_model("X -> Y")
  d <- collapse_data(make_data(model, n = 8), model)

  sd_with <- CausalQueries:::prep_stan_data(model, d)
  sd_without <- CausalQueries:::prep_stan_data(model, d[, c("event", "count")])

  expect_equal(sd_with$n_data, 4L)
  expect_equal(ncol(sd_with$E), 4L)
  expect_equal(ncol(sd_with$map), 4L)
  expect_equal(nrow(sd_with$map), 4L)
  expect_equal(nrow(sd_with$E), sd_with$n_events)
  expect_equal(length(sd_with$Y), sd_with$n_events)

  expect_equal(sd_without$n_data, sd_with$n_data)
  expect_equal(dim(sd_without$map), dim(sd_with$map))
  expect_equal(ncol(sd_without$E), ncol(sd_with$E))
  expect_equal(sd_without$n_events, sd_with$n_events)
  expect_equal(dim(sd_without$E), dim(sd_with$E))

  d_part <- collapse_data(data.frame(X = c(0, 1, NA), Y = c(0, 0, 1)), model)
  sd_part <- CausalQueries:::prep_stan_data(model, d_part)
  expect_equal(sd_part$n_data, 4L)
  expect_equal(ncol(sd_part$E), 4L)
  expect_equal(ncol(sd_part$map), 4L)
  expect_equal(sd_part$n_events, 6L)
  expect_equal(max(sd_part$strategy_ends), 6L)
  expect_equal(nrow(sd_part$E), sd_part$n_events)

  sd_part_no_strategy <- CausalQueries:::prep_stan_data(
    model, d_part[, c("event", "count")])
  expect_equal(sd_part_no_strategy$n_data, 4L)
  expect_equal(dim(sd_part_no_strategy$map), dim(sd_part$map))
  expect_equal(ncol(sd_part_no_strategy$E), 4L)
})
