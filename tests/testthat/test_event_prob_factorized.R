testthat::test_that("event_prob_factorized matches legacy get_event_probabilities", {
  models <- list(
    make_model("X -> Y", legacy = TRUE),
    make_model("X -> M -> Y", legacy = TRUE),
    make_model("X -> Y <- Z", legacy = TRUE),
    make_model("X -> Y; X <-> Y", legacy = TRUE),
    make_model("X -> M -> Y; M <-> Y", legacy = TRUE)
  )

  for (m in models) {
    params <- get_parameters(m)
    legacy <- get_event_probabilities(m, parameters = params)
    fact <- event_prob_factorized(m, parameters = params)
    expect_equal(as.numeric(fact), as.numeric(legacy[rownames(fact), , drop = FALSE]),
                 tolerance = 1e-10)
    expect_equal(sum(fact), 1, tolerance = 1e-10)
  }
})

testthat::test_that("event_prob_factorized given= matches legacy", {
  m <- make_model("X -> Y", legacy = TRUE)
  params <- get_parameters(m)
  legacy <- get_event_probabilities(m, parameters = params, given = "X==1")
  fact <- event_prob_factorized(m, parameters = params, given = "X==1")
  expect_equal(as.numeric(fact), as.numeric(legacy[rownames(fact), , drop = FALSE]),
               tolerance = 1e-10)
})

testthat::test_that("make_parmap_factorized matches make_parmap (incl confound)", {
  for (stmt in c("X -> M -> Y", "X->Y; X<->Y", "X -> M -> Y; M <-> Y")) {
    m <- make_model(stmt, legacy = TRUE)
    legacy_pm <- make_parmap(m)
    fact_pm <- make_parmap_factorized(m)
    expect_equal(dim(fact_pm), dim(legacy_pm))
    expect_equal(as.numeric(fact_pm), as.numeric(legacy_pm))
    expect_equal(as.numeric(attr(fact_pm, "map")), as.numeric(attr(legacy_pm, "map")))
  }
})

testthat::test_that("factorized path works without causal_types attached", {
  m <- make_model("X -> Y", legacy = FALSE)
  expect_null(m$causal_types)
  params <- get_parameters(m)
  w <- event_prob_factorized(m, parameters = params)
  expect_equal(sum(w), 1, tolerance = 1e-10)
  expect_true(all(w >= 0))
})
