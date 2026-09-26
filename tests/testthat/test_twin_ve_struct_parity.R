# Structural twin VE (ve_struct) — exact parameter parity vs grid

testthat::test_that("M1 lookup matches realise_outcomes on one-row types", {
  m <- make_model("X -> Y", legacy = FALSE)
  nt <- get_nodal_types(m, collapse = TRUE)
  for (tau in as.character(nt$Y)) {
    for (x in 0:1) {
      got <- CausalQueries:::child_value_from_nodal_type(tau, x)
      ct <- data.frame(X = as.character(x), Y = tau, stringsAsFactors = FALSE)
      rownames(ct) <- "t"
      mm <- m
      mm$causal_types <- ct
      mm$.cache <- new.env(parent = emptyenv())
      real <- realise_outcomes(mm, add_rownames = TRUE)
      expect_equal(got, as.integer(real$Y[[1]]))
    }
  }
})

testthat::test_that("M2 admits flat TE and rejects nested do", {
  m <- make_model("X -> Y", legacy = FALSE)
  a <- CausalQueries:::twin_world_admit(m, "Y[X=1] - Y[X=0]", "ALL")
  expect_true(a$ok)
  expect_equal(length(a$worlds), 2L)

  nested <- "Y[A=1, B=B[A=0], C=0]"
  m2 <- make_model("A -> Y <- B; A -> B -> Y; C -> Y", legacy = FALSE)
  b <- CausalQueries:::twin_world_admit(m2, nested, "ALL")
  expect_false(b$ok)
  expect_equal(b$reason, "nested_do")
})

testthat::test_that("M2 admits confound when supported (M6)", {
  m <- make_model("X -> Y; X <-> Y", legacy = FALSE)
  a0 <- CausalQueries:::twin_world_admit(
    m, "Y[X=1] - Y[X=0]", "ALL", confound_supported = FALSE
  )
  expect_false(a0$ok)
  expect_equal(a0$reason, "confound_before_M6")
  a1 <- CausalQueries:::twin_world_admit(
    m, "Y[X=1] - Y[X=0]", "ALL", confound_supported = TRUE
  )
  expect_true(a1$ok)
})

testthat::test_that("ve_struct matches grid on ATE (parameters)", {
  m <- make_model("X -> Y", legacy = FALSE)
  params <- get_parameters(m)
  q <- "Y[X=1] - Y[X=0]"
  g <- query_distribution(m, q, using = "parameters", parameters = params,
                          query_eval = "grid")
  s <- query_distribution(m, q, using = "parameters", parameters = params,
                          query_eval = "ve_struct")
  expect_equal(as.numeric(s), as.numeric(g), tolerance = 1e-10)
})

testthat::test_that("ve_struct matches grid on unit-level TE and givens", {
  m <- make_model("X -> Y", legacy = FALSE)
  params <- get_parameters(m)
  q <- "Y[X=1] > Y[X=0]"
  for (gv in list("ALL", "X==1", "Y[X=1]==0")) {
    g <- query_distribution(m, q, given = gv, using = "parameters",
                            parameters = params, query_eval = "grid")
    s <- query_distribution(m, q, given = gv, using = "parameters",
                            parameters = params, query_eval = "ve_struct")
    expect_equal(as.numeric(s), as.numeric(g), tolerance = 1e-10)
  }
})

testthat::test_that("ve_struct matches grid on chain TE", {
  m <- make_model("A -> B -> C -> D", legacy = FALSE)
  params <- get_parameters(m)
  q <- "D[A=1] - D[A=0]"
  g <- query_distribution(m, q, using = "parameters", parameters = params,
                          query_eval = "grid")
  s <- query_distribution(m, q, using = "parameters", parameters = params,
                          query_eval = "ve_struct")
  expect_equal(as.numeric(s), as.numeric(g), tolerance = 1e-10)
})

testthat::test_that("ve_struct matches grid on X->M->Y", {
  m <- make_model("X -> M -> Y", legacy = FALSE)
  params <- get_parameters(m)
  q <- "Y[X=1] - Y[X=0]"
  g <- query_distribution(m, q, using = "parameters", parameters = params,
                          query_eval = "grid")
  s <- query_distribution(m, q, using = "parameters", parameters = params,
                          query_eval = "ve_struct")
  expect_equal(as.numeric(s), as.numeric(g), tolerance = 1e-10)
})

testthat::test_that("ve_struct falls back visibly on nested do", {
  statement <- "A -> Y <- B; A -> B -> Y; C -> Y"
  m <- make_model(statement, legacy = FALSE)
  params <- get_parameters(m)
  q <- "Y[A=1, B=B[A=0], C=0]"
  g <- query_distribution(m, q, using = "parameters", parameters = params,
                          query_eval = "grid")
  expect_message(
    s <- query_distribution(m, q, using = "parameters", parameters = params,
                            query_eval = "ve_struct"),
    "ve_struct"
  )
  expect_equal(as.numeric(s), as.numeric(g), tolerance = 1e-10)
})

testthat::test_that("ve_struct matches grid on confound ATE (parameters)", {
  m <- make_model("X -> Y; X <-> Y", legacy = FALSE)
  params <- get_parameters(m)
  q <- te("X", "Y")
  g <- query_distribution(m, q, using = "parameters", parameters = params,
                          query_eval = "grid")
  s <- query_distribution(m, q, using = "parameters", parameters = params,
                          query_eval = "ve_struct")
  expect_equal(as.numeric(s), as.numeric(g), tolerance = 1e-10)
})

testthat::test_that("ve_struct matches grid on confound + observational given", {
  m <- make_model("X -> Y; X <-> Y", legacy = FALSE)
  params <- get_parameters(m)
  q <- "Y[X=1] - Y[X=0]"
  g <- query_distribution(m, q, given = "Y==1", using = "parameters",
                          parameters = params, query_eval = "grid")
  s <- query_distribution(m, q, given = "Y==1", using = "parameters",
                          parameters = params, query_eval = "ve_struct")
  expect_equal(as.numeric(s), as.numeric(g), tolerance = 1e-10)
})

testthat::test_that("ve_struct matches grid on IV model (parameters)", {
  m <- make_model("Z -> X -> Y; X <-> Y", legacy = FALSE) |>
    set_restrictions(decreasing("Z", "X"))
  params <- get_parameters(m)
  q <- "Y[X=1] - Y[X=0]"
  g <- query_distribution(m, q, using = "parameters", parameters = params,
                          query_eval = "grid")
  s <- query_distribution(m, q, using = "parameters", parameters = params,
                          query_eval = "ve_struct")
  expect_equal(as.numeric(s), as.numeric(g), tolerance = 1e-10)
})

testthat::test_that("ve_struct matches grid on IV with a single prior draw", {
  m <- make_model("Z -> X -> Y; X <-> Y", legacy = FALSE) |>
    set_restrictions(decreasing("Z", "X"))
  set.seed(1)
  m <- set_prior_distribution(m, n_draws = 1)
  pr <- as.numeric(m$prior_distribution[1, ])
  names(pr) <- colnames(m$prior_distribution)
  q <- "Y[X=1] > Y[X=0]"
  g <- query_distribution(m, q, using = "parameters", parameters = pr,
                          query_eval = "grid")
  s <- query_distribution(m, q, using = "parameters", parameters = pr,
                          query_eval = "ve_struct")
  expect_equal(as.numeric(s), as.numeric(g), tolerance = 1e-10)
})

testthat::test_that("ve_struct matches grid under prior draws (batched)", {
  m <- make_model("X -> M -> Y", legacy = FALSE)
  set.seed(2)
  m <- set_prior_distribution(m, n_draws = 40)
  q <- "Y[X=1] - Y[X=0]"
  g <- query_distribution(m, q, using = "priors", query_eval = "grid")
  s <- query_distribution(m, q, using = "priors", query_eval = "ve_struct")
  expect_equal(as.numeric(s[[1]]), as.numeric(g[[1]]), tolerance = 1e-10)
})

testthat::test_that("ve_struct matches grid under posterior draws (batched)", {
  skip_on_cran()
  m <- make_model("X -> Y", legacy = FALSE)
  set.seed(3)
  dat <- make_data(m, n = 30)
  m <- update_model(m, dat, iter = 200, warmup = 100, chains = 1,
                    refresh = 0, seed = 3)
  q <- "Y[X=1] - Y[X=0]"
  g <- query_distribution(m, q, using = "posteriors", query_eval = "grid")
  s <- query_distribution(m, q, using = "posteriors", query_eval = "ve_struct")
  expect_equal(as.numeric(s[[1]]), as.numeric(g[[1]]), tolerance = 1e-10)
})

testthat::test_that("ve_struct matches grid under monotone restrictions", {
  m <- suppressMessages(make_model("X -> M -> Y", monotone = "m", legacy = FALSE))
  params <- get_parameters(m)
  q <- "Y[X=1] - Y[X=0]"
  g <- query_distribution(m, q, using = "parameters", parameters = params,
                          query_eval = "grid")
  s <- query_distribution(m, q, using = "parameters", parameters = params,
                          query_eval = "ve_struct")
  expect_equal(as.numeric(s), as.numeric(g), tolerance = 1e-10)
})

testthat::test_that("query_model ve_struct matches grid mean", {
  m <- make_model("X -> Y", legacy = FALSE)
  params <- get_parameters(m)
  qg <- query_model(m, "Y[X=1] - Y[X=0]", using = "parameters",
                    parameters = list(params), query_eval = "grid")
  qs <- query_model(m, "Y[X=1] - Y[X=0]", using = "parameters",
                    parameters = list(params), query_eval = "ve_struct")
  expect_equal(qs$mean, qg$mean, tolerance = 1e-10)
})

testthat::test_that("auto never selects ve_struct", {
  m <- make_model("X -> Y", legacy = FALSE)
  choice <- CausalQueries:::choose_factorized_query_eval(
    m, "Y[X=1] - Y[X=0]", "ALL", "auto"
  )
  expect_true(choice$method %in% c("grid", "ve"))
  expect_false(identical(choice$method, "ve_struct"))
})
