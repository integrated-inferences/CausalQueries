testthat::test_that("prep_stan_data_factorized has expected dims (no P)", {
  m <- make_model("X -> Y", legacy = FALSE)
  data <- data.frame(
    event = c("X0Y0", "X1Y0", "X0Y1", "X1Y1"),
    count = as.integer(c(1, 1, 1, 1))
  )
  sd <- prep_stan_data_factorized(m, data)
  expect_null(sd$P)
  expect_equal(sd$n_params, 6L)
  expect_equal(sd$n_paths, 4L)
  expect_equal(sd$n_data, 4L)
  expect_equal(nrow(sd$parmap), 6L)
  expect_equal(ncol(sd$parmap), 4L)
})

testthat::test_that("factorized and legacy event probs agree used as Stan w", {
  m <- make_model("X -> Y", legacy = TRUE)
  params <- get_parameters(m)
  pf <- make_parmap_factorized(m)
  x <- rowsum(pf * params, group = m$parameters_df$node, reorder = FALSE)
  w_f <- as.numeric(apply(x, 2, prod))
  names(w_f) <- colnames(pf)
  w_l <- as.numeric(get_event_probabilities(m, parameters = params))
  names(w_l) <- rownames(get_event_probabilities(m, parameters = params))
  expect_equal(w_f[names(w_l)], w_l, tolerance = 1e-10)
})

# MCMC parity — compile factorized Stan on first use (can be slow)
testthat::test_that("factorized update_model returns stamped posterior", {
  skip_if_not(requireNamespace("rstan", quietly = TRUE))
  m <- make_model("X -> Y", legacy = FALSE)
  data <- make_data(make_model("X -> Y", legacy = TRUE), n = 20)
  set.seed(1)
  out <- suppressWarnings(update_model(
    m,
    data = data,
    legacy = FALSE,
    keep_type_distribution = FALSE,
    iter = 400,
    warmup = 200,
    chains = 1,
    refresh = 0,
    seed = 1
  ))
  expect_false(isTRUE(out$legacy))
  expect_false(is.null(out$stan_objects$legacy))
  expect_false(out$stan_objects$legacy)
  expect_null(out$stan_objects$type_posterior)
  expect_equal(ncol(out$posterior_distribution), 6L)
})

testthat::test_that("factorized vs legacy query parameters agree on ATE", {
  m_l <- make_model("X -> Y", legacy = TRUE)
  m_f <- make_model("X -> Y", legacy = FALSE)
  params <- get_parameters(m_l)
  q_l <- query_distribution(m_l, te("X", "Y"), using = "parameters",
                            parameters = params, legacy = TRUE)
  q_f <- query_distribution(m_f, te("X", "Y"), using = "parameters",
                            parameters = params, legacy = FALSE)
  expect_equal(as.numeric(q_f), as.numeric(q_l), tolerance = 1e-10)

  qm_l <- query_model(m_l, te("X", "Y"), using = "parameters",
                      parameters = list(params), legacy = TRUE)
  qm_f <- query_model(m_f, te("X", "Y"), using = "parameters",
                      parameters = list(params), legacy = FALSE)
  expect_equal(qm_f$mean, qm_l$mean, tolerance = 1e-10)
})

testthat::test_that("factorized vs legacy agree on nested do query", {
  statement <- "A -> Y <- B; A -> B -> Y; C -> Y"
  m_l <- make_model(statement, legacy = TRUE)
  m_f <- make_model(statement, legacy = FALSE)
  params <- get_parameters(m_l)
  q <- "Y[A=1, B=B[A=0], C=0]"
  q_l <- query_distribution(m_l, q, using = "parameters",
                            parameters = params, legacy = TRUE)
  q_f <- query_distribution(m_f, q, using = "parameters",
                            parameters = params, legacy = FALSE)
  expect_equal(as.numeric(q_f), as.numeric(q_l), tolerance = 1e-10)
})

testthat::test_that("factorized vs legacy agree on confound ATE", {
  m_l <- make_model("X -> Y; X <-> Y", legacy = TRUE)
  m_f <- make_model("X -> Y; X <-> Y", legacy = FALSE)
  params <- get_parameters(m_l)
  q_l <- query_distribution(m_l, te("X", "Y"), using = "parameters",
                            parameters = params, legacy = TRUE)
  q_f <- query_distribution(m_f, te("X", "Y"), using = "parameters",
                            parameters = params, legacy = FALSE)
  expect_equal(as.numeric(q_f), as.numeric(q_l), tolerance = 1e-10)

  tn <- CausalQueries:::query_type_nodes(m_f, te("X", "Y"), "ALL")
  expect_equal(tn, c("X", "Y"))
})

testthat::test_that("VE relevant set drops intervened-only roots", {
  m <- make_model("X -> Y", legacy = FALSE)
  tn <- CausalQueries:::query_type_nodes(m, "Y[X=1] - Y[X=0]", "ALL")
  expect_equal(tn, "Y")

  m2 <- make_model("A -> B -> C -> D", legacy = FALSE)
  tn2 <- CausalQueries:::query_type_nodes(m2, "D[A=1] - D[A=0]", "ALL")
  expect_equal(tn2, c("B", "C", "D"))

  m3 <- make_model("A -> Y <- B; A -> B -> Y; C -> Y", legacy = FALSE)
  tn3 <- CausalQueries:::query_type_nodes(
    m3, "Y[A=1, B=B[A=0], C=0]", "ALL"
  )
  expect_equal(tn3, c("B", "Y"))
})
