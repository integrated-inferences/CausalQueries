# Factorized event-prob VE vs legacy / full-grid
# (complete, coarsened, confound, given=, prep guards, large-n smoke)
# Always compare VE vs legacy get_event_probabilities and/or full-grid
# event_prob_factorized where applicable.

tol <- 1e-10

# ---------------------------------------------------------------------------
# Phase A: complete assignments — product of nodal factors
# ---------------------------------------------------------------------------

testthat::test_that("Phase A: complete VE matches legacy and full-grid factorized", {
  stmts <- c(
    "X -> Y",
    "X -> M -> Y",
    "X -> Y <- Z",
    "X -> M -> Y <- Z"
  )

  for (stmt in stmts) {
    m_leg <- make_model(stmt, legacy = TRUE)
    m_fac <- make_model(stmt, legacy = FALSE)
    params <- get_parameters(m_leg)

    legacy <- get_event_probabilities(m_leg, parameters = params)
    grid_fac <- event_prob_factorized(m_fac, parameters = params)
    ve <- event_prob_ve_complete(m_fac, parameters = params)

    expect_equal(sum(ve), 1, tolerance = tol, info = stmt)
    expect_equal(
      as.numeric(ve),
      as.numeric(legacy[rownames(ve), , drop = FALSE]),
      tolerance = tol,
      info = paste(stmt, "vs legacy")
    )
    expect_equal(
      as.numeric(ve),
      as.numeric(grid_fac[rownames(ve), , drop = FALSE]),
      tolerance = tol,
      info = paste(stmt, "vs full-grid factorized")
    )

    # Spot-check one complete row via factor_graph product
    g <- factor_graph(m_fac, params)
    row <- complete_data_grid(m_fac)[1, m_fac$nodes, drop = FALSE]
    p_prod <- prob_complete_from_graph(
      g, as.list(stats::setNames(as.integer(unlist(row)), m_fac$nodes))
    )
    expect_equal(p_prod, as.numeric(ve[1, 1]), tolerance = tol, info = stmt)
  }
})

testthat::test_that("Phase A: confound joint factor matches legacy completes", {
  for (stmt in c("X -> Y; X <-> Y", "X -> M -> Y; M <-> Y")) {
    m_leg <- make_model(stmt, legacy = TRUE)
    m_fac <- make_model(stmt, legacy = FALSE)
    params <- get_parameters(m_leg)
    legacy <- get_event_probabilities(m_leg, parameters = params)
    ve <- event_prob_ve_complete(m_fac, parameters = params)
    expect_equal(
      as.numeric(ve),
      as.numeric(legacy[rownames(ve), , drop = FALSE]),
      tolerance = tol,
      info = stmt
    )
    g <- factor_graph(m_fac, params)
    expect_true(isTRUE(g$confounded))
  }
})

# ---------------------------------------------------------------------------
# Phase B: coarsened / missing — sum out latents
# ---------------------------------------------------------------------------

testthat::test_that("Phase B: coarsened VE matches full-grid sum and legacy marginal", {
  cases <- list(
    list(stmt = "X -> Y", evidence = list(Y = 1L)),
    list(stmt = "X -> Y", evidence = list(X = 0L)),
    list(stmt = "X -> M -> Y", evidence = list(Y = 1L)),
    list(stmt = "X -> M -> Y", evidence = list(X = 1L, Y = 0L)),
    list(stmt = "X -> M -> Y", evidence = list(M = 1L)),
    list(stmt = "X -> Y <- Z", evidence = list(Y = 1L)),
    list(stmt = "X -> Y <- Z", evidence = list(X = 1L, Z = 0L)),
    list(stmt = "X -> M -> Y <- Z", evidence = list(Y = 1L)),
    list(stmt = "X -> M -> Y <- Z", evidence = list(X = 0L, Z = 1L))
  )

  for (cs in cases) {
    m_leg <- make_model(cs$stmt, legacy = TRUE)
    m_fac <- make_model(cs$stmt, legacy = FALSE)
    params <- get_parameters(m_leg)
    info <- paste(cs$stmt, paste(names(cs$evidence), cs$evidence, sep = "=", collapse = ","))

    p_ve <- prob_event_ve(m_fac, parameters = params, evidence = cs$evidence)
    p_grid <- prob_coarsened_from_grid(m_fac, params, cs$evidence)

    # Legacy: sum complete patterns matching evidence (not given= renormalization)
    w_leg <- get_event_probabilities(m_leg, parameters = params)
    grid <- get_all_data_types(m_leg, complete_data = TRUE)
    keep <- rep(TRUE, nrow(grid))
    for (nm in names(cs$evidence)) {
      keep <- keep & (as.integer(grid[[nm]]) == as.integer(cs$evidence[[nm]]))
    }
    p_leg <- sum(as.numeric(w_leg)[keep])

    expect_equal(p_ve, p_grid, tolerance = tol, info = paste(info, "VE vs grid"))
    expect_equal(p_ve, p_leg, tolerance = tol, info = paste(info, "VE vs legacy"))
    expect_true(p_ve >= 0 && p_ve <= 1 + tol, info = info)
  }
})

testthat::test_that("Phase B: empty evidence (full marginal) is 1", {
  m <- make_model("X -> M -> Y", legacy = FALSE)
  expect_equal(prob_event_ve(m, evidence = list()), 1, tolerance = tol)
})

testthat::test_that("Phase B: complete evidence equals one grid cell", {
  m_leg <- make_model("X -> Y", legacy = TRUE)
  m_fac <- make_model("X -> Y", legacy = FALSE)
  params <- get_parameters(m_leg)
  grid <- complete_data_grid(m_fac)
  for (i in seq_len(nrow(grid))) {
    ev <- as.list(grid[i, m_fac$nodes, drop = FALSE])
    p_ve <- prob_event_ve(m_fac, parameters = params, evidence = ev)
    p_leg <- as.numeric(get_event_probabilities(m_leg, parameters = params)[
      as.character(grid$event[i]), 1
    ])
    expect_equal(p_ve, p_leg, tolerance = tol)
  }
})

testthat::test_that("Phase B: given= conditional matches legacy after VE unnormalized", {
  # P(Y=1 | X=1) = P(X=1,Y=1) / P(X=1)
  m_leg <- make_model("X -> M -> Y", legacy = TRUE)
  m_fac <- make_model("X -> M -> Y", legacy = FALSE)
  params <- get_parameters(m_leg)

  p_xy <- prob_event_ve(m_fac, params, list(X = 1L, Y = 1L))
  p_x <- prob_event_ve(m_fac, params, list(X = 1L))
  p_cond_ve <- p_xy / p_x

  # Legacy given= renormalizes complete patterns with X==1; P(Y=1|X=1) =
  # mass on Y==1 within that slice
  w_g <- get_event_probabilities(m_leg, parameters = params, given = "X==1")
  grid <- get_all_data_types(m_leg, complete_data = TRUE)
  i <- match(rownames(w_g), rownames(grid))
  p_cond_leg <- sum(as.numeric(w_g)[as.integer(grid$Y[i]) == 1L])

  expect_equal(p_cond_ve, p_cond_leg, tolerance = tol)
})

# ---------------------------------------------------------------------------
# Phase C: prep grid guard + sparse E parity
# ---------------------------------------------------------------------------

testthat::test_that("Phase C: sparse E matches dense families", {
  m <- make_model("X -> M -> Y", legacy = FALSE)
  dense <- get_data_families_factorized(m, sparse = FALSE)
  sparse <- get_data_families_factorized(m, sparse = TRUE)
  expect_equal(dim(sparse), dim(dense))
  expect_equal(as.matrix(sparse[, -(1:2)]), as.matrix(dense[, -(1:2)]))
  expect_equal(sparse$event, dense$event)
  expect_equal(sparse$strategy, dense$strategy)
})

testthat::test_that("Phase C: grid max guard trips on large n", {
  old <- getOption("CausalQueries.factorized_grid_max")
  on.exit(options(CausalQueries.factorized_grid_max = old), add = TRUE)
  options(CausalQueries.factorized_grid_max = 4L) # 2^2
  m <- make_model("X -> M -> Y", legacy = FALSE) # 2^3 = 8
  expect_error(get_data_families_factorized(m), "factorized_grid_max")
  expect_error(make_parmap_factorized(m), "factorized_grid_max")
})

testthat::test_that("Phase C: prep under cap matches legacy E %*% structure", {
  m_leg <- make_model("X -> Y", legacy = TRUE)
  m_fac <- make_model("X -> Y", legacy = FALSE)
  set.seed(1)
  d <- collapse_data(make_data(m_fac, n = 20), m_fac)
  sd_f <- prep_stan_data_factorized(m_fac, d)
  sd_l <- prep_stan_data(m_leg, d)
  expect_equal(sd_f$n_data, sd_l$n_data)
  expect_equal(dim(sd_f$E), dim(sd_l$E))
  expect_equal(as.numeric(sd_f$E), as.numeric(sd_l$E))
})

# ---------------------------------------------------------------------------
# Phase D: confound coarsened vs legacy
# ---------------------------------------------------------------------------

testthat::test_that("Phase D: confound coarsened VE matches legacy marginal", {
  cases <- list(
    list(stmt = "X -> Y; X <-> Y", evidence = list(Y = 1L)),
    list(stmt = "X -> Y; X <-> Y", evidence = list(X = 0L, Y = 1L)),
    list(stmt = "X -> M -> Y; M <-> Y", evidence = list(Y = 1L)),
    list(stmt = "X -> M -> Y; M <-> Y", evidence = list(X = 1L)),
    list(stmt = "X -> M -> Y; M <-> Y", evidence = list(M = 0L, Y = 1L))
  )
  for (cs in cases) {
    m_leg <- make_model(cs$stmt, legacy = TRUE)
    m_fac <- make_model(cs$stmt, legacy = FALSE)
    params <- get_parameters(m_leg)
    info <- paste(cs$stmt, paste(names(cs$evidence), cs$evidence, sep = "=", collapse = ","))

    p_ve <- prob_event_ve(m_fac, parameters = params, evidence = cs$evidence)
    p_grid <- prob_coarsened_from_grid(m_fac, params, cs$evidence)

    w_leg <- get_event_probabilities(m_leg, parameters = params)
    grid <- get_all_data_types(m_leg, complete_data = TRUE)
    keep <- rep(TRUE, nrow(grid))
    for (nm in names(cs$evidence)) {
      keep <- keep & (as.integer(grid[[nm]]) == as.integer(cs$evidence[[nm]]))
    }
    p_leg <- sum(as.numeric(w_leg)[keep])

    expect_equal(p_ve, p_grid, tolerance = tol, info = paste(info, "VE vs grid"))
    expect_equal(p_ve, p_leg, tolerance = tol, info = paste(info, "VE vs legacy"))
  }
})

# ---------------------------------------------------------------------------
# Phase E: observational given via data VE + public get_event_probabilities
# ---------------------------------------------------------------------------

testthat::test_that("Phase E: get_event_probabilities factorized matches legacy (given=)", {
  for (stmt in c("X -> Y", "X -> M -> Y", "X -> Y; X <-> Y")) {
    m_leg <- make_model(stmt, legacy = TRUE)
    m_fac <- make_model(stmt, legacy = FALSE)
    params <- get_parameters(m_leg)
    for (g in list(NULL, "X==1", "Y==1", "X==1 & Y==0")) {
      if (stmt == "X -> Y; X <-> Y" && identical(g, "X==1 & Y==0")) {
        next
      }
      leg <- get_event_probabilities(m_leg, parameters = params, given = g)
      fac <- get_event_probabilities(m_fac, parameters = params, given = g)
      expect_equal(
        as.numeric(fac),
        as.numeric(leg[rownames(fac), , drop = FALSE]),
        tolerance = tol,
        info = paste(stmt, g)
      )
    }
  }
})

testthat::test_that("Phase E: prob_given_ve matches legacy mass of given", {
  m_leg <- make_model("X -> M -> Y", legacy = TRUE)
  m_fac <- make_model("X -> M -> Y", legacy = FALSE)
  params <- get_parameters(m_leg)
  for (g in c("Y==1", "X==1 & Y==0", "M==1")) {
    p_ve <- prob_given_ve(m_fac, params, g)
    w <- get_event_probabilities(m_leg, parameters = params)
    grid <- get_all_data_types(m_leg, complete_data = TRUE)
    matches <- with(grid, eval(parse(text = g)))
    p_leg <- sum(as.numeric(w)[matches])
    expect_equal(p_ve, p_leg, tolerance = tol, info = g)
  }
})

testthat::test_that("Phase E: query_model observational given factorized vs legacy", {
  m_leg <- make_model("X -> Y", legacy = TRUE)
  m_fac <- make_model("X -> Y", legacy = FALSE)
  q <- "Y[X=1] - Y[X=0]"
  for (g in c("Y==1", "X==1 & Y==1")) {
    leg <- query_model(m_leg, query = q, given = g, using = "parameters")
    fac <- query_model(m_fac, query = q, given = g, using = "parameters")
    expect_equal(fac$mean, leg$mean, tolerance = tol, info = g)
  }
})

# ---------------------------------------------------------------------------
# Phase F: large-n smoke — VE without materializing 2^n
# ---------------------------------------------------------------------------

testthat::test_that("Phase F: long chain, observe ends — VE finishes without full grid", {
  # n=12 -> 4096 completes at the default cap; VE with 2 observed is cheap
  nodes <- paste0("V", 1:12)
  stmt <- paste(nodes, collapse = " -> ")
  m <- make_model(stmt, legacy = FALSE)
  params <- get_parameters(m)
  # Should not build 2^12 for this call
  p <- prob_event_ve(
    m, parameters = params,
    evidence = list(V1 = 1L, V12 = 0L)
  )
  expect_true(is.finite(p) && p >= 0 && p <= 1)

  # Against a truncated comparison: same model with n=4 chain vs enum
  m4 <- make_model("V1 -> V2 -> V3 -> V4", legacy = FALSE)
  m4l <- make_model("V1 -> V2 -> V3 -> V4", legacy = TRUE)
  pr <- get_parameters(m4l)
  p4 <- prob_event_ve(m4, pr, list(V1 = 1L, V4 = 0L))
  w <- get_event_probabilities(m4l, parameters = pr)
  grid <- get_all_data_types(m4l, complete_data = TRUE)
  p4_leg <- sum(as.numeric(w)[grid$V1 == 1 & grid$V4 == 0])
  expect_equal(p4, p4_leg, tolerance = tol)
})
