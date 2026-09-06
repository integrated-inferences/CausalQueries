testthat::test_that("legacy flag: default is factorized; TRUE keeps causal types", {
  withr::with_options(list(CausalQueries.legacy = NULL), {
    m_new <- make_model("X -> Y", legacy = FALSE)
    expect_false(isTRUE(m_new$legacy))
    expect_null(m_new$causal_types)

    m_old <- make_model("X -> Y", legacy = TRUE)
    expect_true(isTRUE(m_old$legacy))
    expect_false(is.null(m_old$causal_types))
  })
})

testthat::test_that("legacy = FALSE query works on small unconfounded models", {
  m <- make_model("X -> Y", legacy = FALSE)
  q <- query_model(m, te("X", "Y"), using = "parameters", legacy = FALSE)
  expect_true(is.data.frame(q) || inherits(q, "data.frame") || nrow(as.data.frame(q)) >= 1)
})

testthat::test_that("legacy = FALSE update/query work with confound", {
  m <- make_model("X -> Y; X <-> Y", legacy = FALSE)
  q <- query_model(m, te("X", "Y"), using = "parameters", legacy = FALSE)
  expect_true(nrow(as.data.frame(q)) >= 1)

  skip_if_not(requireNamespace("rstan", quietly = TRUE))
  data <- make_data(make_model("X -> Y; X <-> Y", legacy = TRUE), n = 20)
  out <- suppressWarnings(update_model(
    m, data = data, legacy = FALSE, iter = 200, warmup = 100,
    chains = 1, refresh = 0, seed = 1
  ))
  expect_false(isTRUE(out$stan_objects$legacy))
  expect_null(out$stan_objects$type_posterior)
  expect_equal(ncol(out$posterior_distribution), 10L)
})

testthat::test_that("later steps inherit model$legacy when arg is NULL", {
  m <- make_model("X -> Y", legacy = TRUE)
  withr::with_options(list(CausalQueries.legacy = FALSE), {
    expect_true(resolve_legacy(NULL, m))
  })
  m2 <- make_model("X -> Y", legacy = FALSE)
  withr::with_options(list(CausalQueries.legacy = TRUE), {
    expect_false(resolve_legacy(NULL, m2))
  })
})

testthat::test_that("explicit legacy override warns when it conflicts with stamp", {
  m <- make_model("X -> Y", legacy = TRUE)
  expect_warning(resolve_legacy(FALSE, m), "overrides model stamp")
})

testthat::test_that("stamp_legacy records fit method for downstream steps", {
  m <- make_model("X -> Y", legacy = TRUE)
  m$stan_objects <- list()
  m <- stamp_legacy(m, TRUE)
  expect_true(m$legacy)
  expect_true(m$stan_objects$legacy)
  m$legacy <- FALSE
  expect_true(resolve_legacy(NULL, m))
})
