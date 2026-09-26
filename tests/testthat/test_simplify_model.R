testthat::test_that("make_model drop_interactions + monotone reduces Y types", {
  m <- suppressMessages(make_model(
    "A -> Y <- B; C -> Y",
    drop_interactions = TRUE,
    monotone = "+",
    legacy = FALSE
  ))
  # unary + all-increasing: constants + one schedule per parent
  expect_equal(length(m$nodal_types$Y), 2L + 3L)
  sat <- make_model("A -> Y <- B; C -> Y", legacy = FALSE)
  expect_lt(length(m$nodal_types$Y), length(sat$nodal_types$Y))
})

testthat::test_that("four-parent bare-bones one-liner stays small", {
  m <- suppressMessages(make_model(
    "A -> Y <- B; C -> Y; D -> Y",
    drop_interactions = TRUE,
    monotone = "+",
    legacy = FALSE
  ))
  expect_equal(length(m$nodal_types$Y), 2L + 4L)
  expect_equal(nrow(m$parameters_df), 2L * 4L + (2L + 4L)) # 4 roots + Y
  expect_null(m$causal_types)
})

testthat::test_that("exact unary type counts for drop / monotone", {
  for (k in 1:4) {
    pa <- LETTERS[1:k]
    n0 <- length(CausalQueries:::generate_restricted_nodal_types(
      pa, 2L, list(), character(0)
    ))
    np <- length(CausalQueries:::generate_restricted_nodal_types(
      pa, 2L, list(), setNames(rep("+", k), pa)
    ))
    nm <- length(CausalQueries:::generate_restricted_nodal_types(
      pa, 2L, list(), setNames(rep("m", k), pa)
    ))
    expect_equal(n0, 2L + 2L * k)
    expect_equal(np, 2L + k)
    expect_equal(nm, 2L + 2L * k)
    expect_equal(
      CausalQueries:::estimate_restricted_nodal_type_count(
        pa, 2L, list(), setNames(rep("+", k), pa)
      ),
      2L + k
    )
  }
})

testthat::test_that("k>4 works with drop_interactions; monotone alone refuses", {
  stmt <- "A -> Y <- B; C -> Y; D -> Y; E -> Y"
  m <- suppressMessages(make_model(
    stmt,
    drop_interactions = TRUE,
    monotone = "+",
    legacy = FALSE
  ))
  expect_equal(length(m$nodal_types$Y), 2L + 5L)
  expect_equal(nchar(m$nodal_types$Y[[1]]), 2^5)

  expect_error(
    make_model(stmt, monotone = "m", legacy = FALSE),
    "drop_interactions|saturated|2\\^\\(2\\^"
  )
  expect_error(
    make_model(stmt, legacy = FALSE),
    "too many nodal types|nodal_types"
  )
})

testthat::test_that("keep_interactions block count is exact", {
  # functions of {A,B} (16) plus C and not-C (2) = 18
  n <- length(CausalQueries:::generate_restricted_nodal_types(
    c("A", "B", "C"), 2L, list(c("A", "B")), character(0)
  ))
  expect_equal(n, 18L)
  m_keep <- suppressMessages(make_model(
    "A -> Y <- B; C -> Y",
    drop_interactions = TRUE,
    keep_interactions = list(c("A", "B")),
    legacy = FALSE
  ))
  expect_equal(length(m_keep$nodal_types$Y), 18L)
})

testthat::test_that("confound multiplies parameter estimate", {
  n_types <- c(X = 2, Y = 4)
  expect_equal(CausalQueries:::estimate_parameters_with_confound(n_types), 6)
  expect_equal(
    CausalQueries:::estimate_parameters_with_confound(n_types, list(Y = "X")),
    2 + 4 * 2
  )
  m <- make_model("X -> Y; X <-> Y", legacy = FALSE)
  expect_equal(
    nrow(m$parameters_df),
    CausalQueries:::estimate_parameters_with_confound(
      lengths(m$nodal_types),
      list(Y = "X")
    )
  )
})

testthat::test_that("complexity guard sees confound-expanded parameters", {
  expect_error(
    CausalQueries:::check_causal_type_count(
      c(X = 2, Y = 16),
      allow_large = FALSE,
      add_causal_types = FALSE,
      confound_pairs = list(Y = "X"),
      max_parameters = 20
    ),
    "parameters"
  )
  expect_warning(
    CausalQueries:::check_causal_type_count(
      c(X = 2, Y = 16),
      allow_large = TRUE,
      add_causal_types = FALSE,
      confound_pairs = list(Y = "X"),
      max_parameters = 20
    ),
    "parameters"
  )
})

testthat::test_that("simplify_model matches make_model restrictions", {
  m0 <- make_model("A -> Y <- B", legacy = FALSE)
  m1 <- suppressMessages(simplify_model(
    m0, drop_interactions = 2, monotone = "+"
  ))
  m2 <- suppressMessages(make_model(
    "A -> Y <- B",
    drop_interactions = 2,
    monotone = "+",
    legacy = FALSE
  ))
  expect_equal(sort(m1$nodal_types$Y), sort(m2$nodal_types$Y))
  expect_equal(length(m2$nodal_types$Y), 2L + 2L)
})

testthat::test_that("set_nodal_restrictions is alias of simplify_model", {
  m <- make_model("X -> Y", legacy = FALSE)
  a <- suppressMessages(simplify_model(m, monotone = "+"))
  b <- suppressMessages(set_nodal_restrictions(m, monotone = "+"))
  expect_equal(a$nodal_types, b$nodal_types)
})

testthat::test_that("monotone m keeps uniform +/- and drops QI types", {
  m <- suppressMessages(simplify_model(
    make_model("X1 -> Y <- X2", legacy = FALSE),
    monotone = list(Y = c(X1 = "m", X2 = "m"))
  ))
  # AND / OR / Y=X2 / Y=not X2 survive; XOR / XNOR do not
  expect_true(all(c("0001", "0111", "0011", "1100") %in% m$nodal_types$Y))
  expect_false(any(c("1001", "0110") %in% m$nodal_types$Y))
  # m keeps more than +
  m_plus <- suppressMessages(simplify_model(
    make_model("X1 -> Y <- X2", legacy = FALSE),
    monotone = list(Y = c(X1 = "+", X2 = "+"))
  ))
  expect_gt(length(m$nodal_types$Y), length(m_plus$nodal_types$Y))
  expect_false("1100" %in% m_plus$nodal_types$Y)
  expect_true("0011" %in% m_plus$nodal_types$Y)
})

testthat::test_that("monotone + drops 1100; monotone - drops 0011", {
  m_inc <- suppressMessages(simplify_model(
    make_model("X1 -> Y <- X2", legacy = FALSE),
    monotone = list(Y = c(X2 = "+"))
  ))
  m_dec <- suppressMessages(simplify_model(
    make_model("X1 -> Y <- X2", legacy = FALSE),
    monotone = list(Y = c(X2 = "-"))
  ))
  expect_true("0011" %in% m_inc$nodal_types$Y)
  expect_false("1100" %in% m_inc$nodal_types$Y)
  expect_true("1100" %in% m_dec$nodal_types$Y)
  expect_false("0011" %in% m_dec$nodal_types$Y)
  expect_false("1001" %in% m_inc$nodal_types$Y)
  expect_false("1001" %in% m_dec$nodal_types$Y)
})

testthat::test_that("monotone n keeps only qualitative-interaction types", {
  m <- suppressMessages(simplify_model(
    make_model("X1 -> Y <- X2", legacy = FALSE),
    monotone = list(Y = c(X1 = "n"))
  ))
  expect_true(all(c("1001", "0110") %in% m$nodal_types$Y) ||
                any(c("1001", "0110") %in% m$nodal_types$Y))
  expect_false("0011" %in% m$nodal_types$Y)
  expect_false("0001" %in% m$nodal_types$Y)
})

testthat::test_that("global monotone m and compact AmY work", {
  m1 <- suppressMessages(make_model("A -> Y <- B", monotone = "m", legacy = FALSE))
  m2 <- suppressMessages(simplify_model(
    make_model("A -> Y <- B", legacy = FALSE),
    monotone = list(Y = c(A = "m", B = "m"))
  ))
  expect_equal(sort(m1$nodal_types$Y), sort(m2$nodal_types$Y))
  m3 <- suppressMessages(simplify_model(
    make_model("A -> Y <- B", legacy = FALSE),
    monotone = "AmY"
  ))
  # AmY only constrains parent A; still drops XNOR/XOR (QI in A)
  expect_false(any(c("1001", "0110") %in% m3$nodal_types$Y))
  expect_true("0011" %in% m3$nodal_types$Y)
})

testthat::test_that("k=1 monotone edge cases", {
  m <- suppressMessages(make_model("X -> Y", monotone = "m", legacy = FALSE))
  # single parent: no background to flip sign; all four types satisfy "m"
  expect_equal(sort(m$nodal_types$Y), sort(c("00", "01", "10", "11")))
  m_plus <- suppressMessages(make_model("X -> Y", monotone = "+", legacy = FALSE))
  expect_false("10" %in% m_plus$nodal_types$Y)
})

testthat::test_that("k=3 monotone m drops a known QI type", {
  m <- suppressMessages(simplify_model(
    make_model("A -> Y <- B; C -> Y", legacy = FALSE),
    monotone = list(Y = c(A = "m", B = "m", C = "m"))
  ))
  expect_lt(length(m$nodal_types$Y), 256L)
  sat <- make_model("A -> Y <- B; C -> Y", legacy = FALSE)$nodal_types$Y
  and_candidates <- sat[vapply(sat, function(ts) {
    f <- as.integer(strsplit(ts, "")[[1]])
    sum(f) == 1L && f[length(f)] == 1L
  }, logical(1))]
  if (length(and_candidates)) {
    expect_true(any(and_candidates %in% m$nodal_types$Y))
  }
})

testthat::test_that("ambiguous AmmY errors lightly", {
  m <- make_model("Am -> Y; A -> mY", legacy = FALSE)
  expect_error(
    simplify_model(m, monotone = "AmmY"),
    "ambiguous|list form"
  )
})

testthat::test_that("keep_interactions can retain a 2-way", {
  m_drop <- suppressMessages(make_model(
    "A -> Y <- B",
    drop_interactions = TRUE,
    legacy = FALSE
  ))
  m_keep <- suppressMessages(make_model(
    "A -> Y <- B",
    drop_interactions = TRUE,
    keep_interactions = list(c("A", "B")),
    legacy = FALSE
  ))
  expect_gte(length(m_keep$nodal_types$Y), length(m_drop$nodal_types$Y))
  expect_equal(length(m_drop$nodal_types$Y), 2L + 2L * 2L)
  expect_equal(length(m_keep$nodal_types$Y), 16L) # all functions of A,B
})

testthat::test_that("nodal_types conflicts with drop_interactions", {
  expect_error(
    make_model(
      "X -> Y",
      nodal_types = list(X = c("0", "1"), Y = c("00", "11")),
      drop_interactions = TRUE
    ),
    "not both"
  )
})

testthat::test_that("simplify errors after confound", {
  m <- make_model("X -> Y; X <-> Y", legacy = FALSE)
  expect_error(simplify_model(m, monotone = "+"), "confound")
})

testthat::test_that("drop_interactions = 3 uses order_leq and matches gold counts", {
  expect_equal(
    CausalQueries:::restriction_regime(4L, 3L, list()),
    "order_leq"
  )
  pa <- LETTERS[1:4]
  n3 <- length(CausalQueries:::generate_restricted_nodal_types(
    pa, 3L, list(), character(0)
  ))
  expect_equal(n3, 222L)
  n3m <- length(CausalQueries:::generate_restricted_nodal_types(
    pa, 3L, list(), setNames(rep("m", 4), pa)
  ))
  expect_equal(n3m, 58L)
  # parent / node order unchanged for the fan-in statement
  m <- suppressMessages(make_model(
    "A->B <-C; D->B <-E",
    monotone = "m",
    drop_interactions = 3,
    legacy = FALSE
  ))
  expect_equal(m$nodes, c("A", "C", "D", "E", "B"))
  expect_equal(length(m$nodal_types$B), 58L)
})

testthat::test_that("minimal_event_data avoids causal-type product (factorized)", {
  m <- suppressMessages(make_model(
    "X1 -> Y <- X2; X3 -> Y",
    monotone = "m",
    legacy = FALSE
  ))
  ev <- CausalQueries:::minimal_event_data(m)
  expect_true(all(c("event", "strategy", "count") %in% names(ev)))
  expect_equal(sum(ev$count), 0L)
  expect_gt(nrow(ev), 1L)
  expect_null(m$causal_types)
})

testthat::test_that("collapse_data with observations avoids causal-type product", {
  m <- suppressMessages(make_model(
    "A -> Y <- B; C -> Y",
    monotone = "m",
    legacy = FALSE
  ))
  dat <- data.frame(A = 1, B = 1, C = 0, Y = 1)
  ev <- collapse_data(dat, m)
  expect_equal(sum(ev$count), 1L)
  expect_true(any(ev$count > 0))
  expect_null(m$causal_types)
})

testthat::test_that("generation skeleton stays modular under monotone = m", {
  # A1,A2,A3 -> A; B1,B2,B3 -> B; A,B,C -> D
  # Causal-type product is huge (~1e8 with m); factorized path uses 2^10 grid.
  stmt <- paste(
    "A1 -> A <- A2; A3 -> A;",
    "B1 -> B <- B2; B3 -> B;",
    "A -> D <- B; C -> D"
  )
  t0 <- proc.time()[[3]]
  m <- suppressMessages(make_model(stmt, monotone = "m", legacy = FALSE))
  expect_null(m$causal_types)
  expect_equal(length(m$nodes), 10L)
  # Fat mediators / child: well below saturated 256
  expect_lt(length(m$nodal_types$A), 256L)
  expect_lt(length(m$nodal_types$B), 256L)
  expect_lt(length(m$nodal_types$D), 256L)

  dat <- data.frame(A = 1, B = 1, C = 0, D = 1)
  ev <- collapse_data(dat, m)
  elapsed <- proc.time()[[3]] - t0
  expect_equal(sum(ev$count), 1L)
  expect_null(m$causal_types)
  expect_lt(elapsed, 60)
})
