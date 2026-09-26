# Speed: ve_struct vs grid when the relevant type product is large

testthat::test_that("ve_struct beats grid on a long chain TE (large product)", {
  skip_on_cran()
  m <- make_model(
    "A -> B -> C -> D -> E -> F -> G -> H -> I",
    legacy = FALSE
  )
  params <- get_parameters(m)
  q <- "I[A=1] - I[A=0]"
  tn <- CausalQueries:::query_type_nodes(m, q, "ALL")
  n_hat <- CausalQueries:::estimate_relevant_type_product(m, tn)
  expect_gt(n_hat, 1e4)

  g <- query_distribution(m, q, using = "parameters", parameters = params,
                          query_eval = "grid")
  s <- query_distribution(m, q, using = "parameters", parameters = params,
                          query_eval = "ve_struct")
  expect_equal(as.numeric(s), as.numeric(g), tolerance = 1e-10)

  t_grid <- system.time({
    query_distribution(m, q, using = "parameters", parameters = params,
                       query_eval = "grid")
  })[["elapsed"]]
  t_struct <- system.time({
    query_distribution(m, q, using = "parameters", parameters = params,
                       query_eval = "ve_struct")
  })[["elapsed"]]

  expect_lt(
    t_struct, t_grid,
    label = sprintf("struct=%.3fs should be < grid=%.3fs (n_hat≈%.0f)",
                    t_struct, t_grid, n_hat)
  )
})

testthat::test_that("ve_struct finishes Trust-like restricted TE quickly", {
  skip_on_cran()
  m <- suppressMessages(make_model(
    paste(
      "Marginalization -> Participation;",
      "Marginalization -> Understanding;",
      "Marginalization -> Inclusion;",
      "Marginalization -> Connections;",
      "Invitation -> Participation;",
      "Municipality -> Participation;",
      "Municipality -> Understanding;",
      "Municipality -> Inclusion;",
      "Municipality -> Connections;",
      "Participation -> Understanding;",
      "Participation -> Connections;",
      "Participation -> Inclusion;",
      "Inclusion -> Trust;",
      "Understanding -> Trust;",
      "Connections -> Trust"
    ),
    monotone = "m",
    drop_interactions = 3,
    legacy = FALSE
  ))
  params <- get_parameters(m)
  q <- "Trust[Marginalization=1] - Trust[Marginalization=0]"
  t0 <- system.time({
    s <- query_distribution(m, q, using = "parameters", parameters = params,
                            query_eval = "ve_struct")
  })[["elapsed"]]
  expect_true(is.finite(as.numeric(s)))
  expect_lt(t0, 30)
})

testthat::test_that("ve_struct priors scales sub-linearly vs naive per-draw cost", {
  skip_on_cran()
  m <- make_model("A -> B -> C -> D -> E", legacy = FALSE)
  set.seed(4)
  m <- set_prior_distribution(m, n_draws = 64)
  q <- "E[A=1] - E[A=0]"

  t1 <- system.time({
    query_distribution(m, q, using = "parameters",
                       parameters = as.numeric(m$prior_distribution[1, ]),
                       query_eval = "ve_struct")
  })[["elapsed"]]
  t64 <- system.time({
    s <- query_distribution(m, q, using = "priors", query_eval = "ve_struct")
  })[["elapsed"]]

  expect_equal(length(as.numeric(s[[1]])), 64L)
  # Batched: 64 draws should be far cheaper than 64× one draw
  expect_lt(t64, max(5, 12 * t1 + 1))
})
