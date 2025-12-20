context("Testing improved query error messages")

testthat::skip_on_cran()

test_that("Query syntax warning message works correctly", {

  # Create a simple model
  model <- make_model("X -> M -> Y; X <-> Y")

  # Test that incorrect syntax (& instead of ,) triggers helpful warning
  expect_warning(
    query_model(model, "Y[X=1 & M=1]"),
    regexp = "Query syntax error detected"
  )

  # Test that the warning message contains helpful information
  warning_msg <- tryCatch(
    query_model(model, "Y[X=1 & M=1]"),
    warning = function(w) w$message
  )

  expect_true(grepl("Did you mean", warning_msg))
  expect_true(grepl("X=1, M=1", warning_msg))

  # Test that correct syntax works without warnings
  expect_no_warning(query_model(model, "Y[X=1, M=1]"))
})

test_that("Correct query syntax works as expected", {

  model <- make_model("X -> M -> Y; X <-> Y")

  # Test various correct syntax patterns
  expect_no_error(query_model(model, "Y[X=1]"))
  expect_no_error(query_model(model, "Y[X=1, M=0]"))

  # Test that logical AND (&) works between different query parts
  expect_no_error(query_model(model, "(Y[X=1] > Y[X=0]) & (M[X=1] == 1)"))
})
