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

testthat::test_that("monotone X+Y removes decreasing schedule", {
  m <- suppressMessages(make_model("X -> Y", monotone = "X+Y", legacy = FALSE))
  expect_false("10" %in% m$nodal_types$Y)
  expect_true(all(c("00", "01", "11") %in% m$nodal_types$Y))
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
