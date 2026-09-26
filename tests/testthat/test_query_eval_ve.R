# Grid vs VE parity (factorized query_eval)

testthat::test_that("causal_types_relevant_slice matches expand.grid order", {
  m <- make_model("A -> B -> C", legacy = FALSE)
  tn <- c("B", "C")
  full <- CausalQueries:::causal_types_relevant(m, tn)
  slice <- CausalQueries:::causal_types_relevant_slice(m, tn, 1L, nrow(full))
  expect_equal(as.matrix(slice), as.matrix(full))
  mid <- CausalQueries:::causal_types_relevant_slice(m, tn, 2L, 5L)
  expect_equal(as.matrix(mid), as.matrix(full[2:5, , drop = FALSE]))
})

testthat::test_that("query_eval ve matches grid on ATE", {
  m <- make_model("X -> Y", legacy = FALSE)
  params <- get_parameters(m)
  q <- "Y[X=1] - Y[X=0]"
  g <- query_distribution(m, q, using = "parameters", parameters = params,
                          query_eval = "grid")
  v <- query_distribution(m, q, using = "parameters", parameters = params,
                          query_eval = "ve")
  expect_equal(as.numeric(v), as.numeric(g), tolerance = 1e-10)
})

testthat::test_that("query_eval ve matches grid on nested do", {
  statement <- "A -> Y <- B; A -> B -> Y; C -> Y"
  m <- make_model(statement, legacy = FALSE)
  params <- get_parameters(m)
  q <- "Y[A=1, B=B[A=0], C=0]"
  # Force small chunks so nested path is exercised across slices
  old <- getOption("CausalQueries.factorized_ve_chunk")
  on.exit(options(CausalQueries.factorized_ve_chunk = old), add = TRUE)
  options(CausalQueries.factorized_ve_chunk = 3L)
  g <- query_distribution(m, q, using = "parameters", parameters = params,
                          query_eval = "grid")
  v <- query_distribution(m, q, using = "parameters", parameters = params,
                          query_eval = "ve")
  expect_equal(as.numeric(v), as.numeric(g), tolerance = 1e-10)
})

testthat::test_that("query_eval ve matches grid on confound ATE", {
  m <- make_model("X -> Y; X <-> Y", legacy = FALSE)
  params <- get_parameters(m)
  q <- te("X", "Y")
  g <- query_distribution(m, q, using = "parameters", parameters = params,
                          query_eval = "grid")
  v <- query_distribution(m, q, using = "parameters", parameters = params,
                          query_eval = "ve")
  expect_equal(as.numeric(v), as.numeric(g), tolerance = 1e-10)
})

testthat::test_that("query_eval ve matches grid on unit-level TE and givens", {
  m <- make_model("X -> Y", legacy = FALSE)
  params <- get_parameters(m)
  q <- "Y[X=1] > Y[X=0]"
  for (gv in list("ALL", "X==1", "Y[X=1]==0")) {
    g <- query_distribution(m, q, given = gv, using = "parameters",
                            parameters = params, query_eval = "grid")
    v <- query_distribution(m, q, given = gv, using = "parameters",
                            parameters = params, query_eval = "ve")
    expect_equal(as.numeric(v), as.numeric(g), tolerance = 1e-10)
  }
})

testthat::test_that("query_eval ve matches grid under monotone restrictions", {
  m <- suppressMessages(make_model("X -> M -> Y", monotone = "m", legacy = FALSE))
  params <- get_parameters(m)
  q <- "Y[X=1] - Y[X=0]"
  old <- getOption("CausalQueries.factorized_ve_chunk")
  on.exit(options(CausalQueries.factorized_ve_chunk = old), add = TRUE)
  options(CausalQueries.factorized_ve_chunk = 4L)
  g <- query_distribution(m, q, using = "parameters", parameters = params,
                          query_eval = "grid")
  v <- query_distribution(m, q, using = "parameters", parameters = params,
                          query_eval = "ve")
  expect_equal(as.numeric(v), as.numeric(g), tolerance = 1e-10)
})

testthat::test_that("query_eval ve matches grid for case_level on priors", {
  m <- make_model("X -> Y", legacy = FALSE) |>
    set_prior_distribution(n_draws = 40)
  q <- "Y[X=1] > Y[X=0]"
  g <- query_distribution(m, q, given = "X==1 & Y==1", using = "priors",
                          case_level = TRUE, query_eval = "grid")
  v <- query_distribution(m, q, given = "X==1 & Y==1", using = "priors",
                          case_level = TRUE, query_eval = "ve")
  expect_equal(as.numeric(v), as.numeric(g), tolerance = 1e-10)
})

testthat::test_that("query_eval auto uses grid when product is small", {
  m <- make_model("X -> Y", legacy = FALSE)
  choice <- CausalQueries:::choose_factorized_query_eval(
    m, "Y[X=1] - Y[X=0]", "ALL", "auto"
  )
  expect_equal(choice$method, "grid")
})

testthat::test_that("query_model respects query_eval = ve", {
  m <- make_model("X -> Y", legacy = FALSE)
  params <- get_parameters(m)
  qg <- query_model(m, "Y[X=1] - Y[X=0]", using = "parameters",
                    parameters = list(params), query_eval = "grid")
  qv <- query_model(m, "Y[X=1] - Y[X=0]", using = "parameters",
                    parameters = list(params), query_eval = "ve")
  expect_equal(qv$mean, qg$mean, tolerance = 1e-10)
})

testthat::test_that("grid error message points to query_eval ve", {
  m <- make_model("A -> B -> C -> D", legacy = FALSE)
  old <- getOption("CausalQueries.factorized_query_max")
  on.exit(options(CausalQueries.factorized_query_max = old), add = TRUE)
  options(CausalQueries.factorized_query_max = 10)
  expect_error(
    query_distribution(m, "D[A=1] - D[A=0]", query_eval = "grid"),
    "query_eval = \"ve\""
  )
})

testthat::test_that("query_eval auto switches to ve when grid cap exceeded", {
  m <- make_model("A -> B -> C -> D", legacy = FALSE)
  old_g <- getOption("CausalQueries.factorized_query_max")
  old_v <- getOption("CausalQueries.factorized_ve_chunk")
  on.exit({
    options(CausalQueries.factorized_query_max = old_g)
    options(CausalQueries.factorized_ve_chunk = old_v)
  }, add = TRUE)
  options(CausalQueries.factorized_query_max = 10)
  options(CausalQueries.factorized_ve_chunk = 8L)
  choice <- CausalQueries:::choose_factorized_query_eval(
    m, "D[A=1] - D[A=0]", "ALL", "auto"
  )
  expect_equal(choice$method, "ve")
  params <- get_parameters(m)
  # auto should complete via ve
  out <- query_distribution(
    m, "D[A=1] - D[A=0]", using = "parameters", parameters = params,
    query_eval = "auto"
  )
  expect_true(is.finite(as.numeric(out)))
})
