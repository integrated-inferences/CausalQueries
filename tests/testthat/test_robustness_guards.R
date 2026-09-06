context("Robustness guards (3.1, 3.3, 3.5, 3.10, 3.11)")

test_that("snippet keeps a one-column data frame (3.11)", {
  df <- data.frame(a = seq_len(20))
  expect_output(CausalQueries:::snippet(df, nc = 10, nr = 5), "snippet")
})


test_that("make_parameters posterior_* uses has_posterior (3.10)", {
  model <- make_model("X -> Y")
  expect_error(
    make_parameters(model, param_type = "posterior_mean"),
    "Posterior distribution required"
  )
  expect_error(
    make_parameters(model, param_type = "posterior_draw"),
    "Posterior distribution required"
  )
})


test_that("`&` inside do-brackets errors (3.5)", {
  model <- make_model("X -> M -> Y")
  expect_error(
    query_model(model, "Y[X=1 & M=1]", using = "parameters"),
    "not allowed inside do-brackets"
  )
  expect_no_error(
    query_model(model, "Y[X=1, M=1]", using = "parameters")
  )
  expect_no_error(
    query_model(model, "(Y[X=1] > Y[X=0]) & (M[X=1] == 1)", using = "parameters")
  )
})


test_that("spaces inside do-brackets still parse (3.6)", {
  model <- make_model("X -> Y")
  expect_no_error(
    query_model(model, "Y[X = 1] - Y[X = 0]", using = "parameters")
  )
})


test_that("set_restrictions warns when nothing matches (3.3)", {
  model <- make_model("X -> Y")
  # Statement matches no nodal types for Y under keep = FALSE -> nothing dropped
  expect_warning(
    out <- set_restrictions(model, statement = "Y[X=1] > Y[X=1]"),
    "matched no parameters"
  )
  expect_identical(out$nodal_types, model$nodal_types)
})


test_that("set_restrictions errors when a node would be emptied (3.3)", {
  model <- make_model("X -> Y")
  expect_error(
    set_restrictions(
      model,
      labels = list(Y = c("00", "10", "01", "11"))
    ),
    "remove all nodal types"
  )
})


test_that("realise_outcomes rejects non-0/1 dos (3.1)", {
  model <- make_model("X -> Y")
  expect_error(
    realise_outcomes(model, dos = list(X = 2)),
    "`dos` values must be 0 or 1"
  )
  expect_error(
    realise_outcomes(model, dos = list(X = TRUE)),
    "`dos` values must be 0 or 1"
  )
  expect_no_error(realise_outcomes(model, dos = list(X = 0)))
  expect_no_error(realise_outcomes(model, dos = list(X = "1")))
})
