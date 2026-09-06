# Extracted from test_inspect.R:119

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "CausalQueries", path = "..")
attach(test_env, warn.conflicts = FALSE)

# prequel ----------------------------------------------------------------------
context("Inspect function check")
testthat::skip_on_cran()
model <- make_model("X -> Y")
data <- make_data(model, n = 4)
model <- update_model(
  model,
  data = data,
  keep_fit = TRUE,
  keep_event_probabilities = TRUE
)
model_legacy_types <- NULL

# test -------------------------------------------------------------------------
skip_if_legacy_objects()
model_legacy_types <<- with_legacy_true({
    update_model(
      make_model("X -> Y"),
      data = data,
      keep_fit = TRUE,
      keep_event_probabilities = TRUE,
      keep_type_distribution = TRUE,
      refresh = 0
    )
  })
expect_output(
    inspect(model_legacy_types, what = "type_posterior"),
    "Posterior draws of causal types \\(transformed parameters\\):"
  )
