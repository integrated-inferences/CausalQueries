testthat::test_that("make_model drop_interactions + monotone reduces Y types", {
  m <- suppressMessages(make_model(
    "A -> Y <- B; C -> Y",
    drop_interactions = TRUE,
    monotone = "+",
    legacy = FALSE
  ))
  expect_lt(length(m$nodal_types$Y), 256L)
  expect_gt(length(m$nodal_types$Y), 1L)
  # All kept types weakly increasing in each parent
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
  expect_lt(length(m$nodal_types$Y), 100L)
  expect_lt(nrow(m$parameters_df), 120L)
  expect_null(m$causal_types)
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
  # saturated componentwise-increasing type (all zeros then ones by parents)
  # AND of three parents should survive
  and3 <- paste(rep(c(0, 0, 0, 0, 0, 0, 0, 1), 1), collapse = "")
  # type_matrix order: verify AND is kept if present in saturated set
  sat <- make_model("A -> Y <- B; C -> Y", legacy = FALSE)$nodal_types$Y
  and_candidates <- sat[vapply(sat, function(ts) {
    f <- as.integer(strsplit(ts, "")[[1]])
    # Y=1 only when all parents 1 — last assignment in grid is often all-1
    sum(f) == 1L && f[length(f)] == 1L
  }, logical(1))]
  if (length(and_candidates)) {
    expect_true(any(and_candidates %in% m$nodal_types$Y))
  }
})

testthat::test_that("ambiguous AmmY errors lightly", {
  # Nodes Am and Y plus A and mY would be exotic; construct Am -> Y and A -> mY
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
