context("Testing improved query error messages")

testthat::skip_on_cran()

test_that("`&` inside do-brackets errors with a correction hint", {

  model <- make_model("X -> M -> Y; X <-> Y")

  expect_error(
    query_model(model, "Y[X=1 & M=1]"),
    regexp = "not allowed inside do-brackets"
  )

  err_msg <- tryCatch(
    query_model(model, "Y[X=1 & M=1]"),
    error = function(e) e$message
  )

  expect_true(grepl("X=1, M=1", err_msg))

  expect_no_error(query_model(model, "Y[X=1, M=1]"))
})

test_that("Correct query syntax works as expected", {

  model <- make_model("X -> M -> Y; X <-> Y")

  expect_no_error(query_model(model, "Y[X=1]"))
  expect_no_error(query_model(model, "Y[X=1, M=0]"))

  # Logical AND between query parts remains valid
  expect_no_error(query_model(model, "(Y[X=1] > Y[X=0]) & (M[X=1] == 1)"))
})
