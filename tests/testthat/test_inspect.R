

context("Inspect function check")

testthat::skip_on_cran()

model <- make_model("X -> Y")
data <- make_data(model, n = 4)


test_that("inspect statement", {
  expect_output(inspect(model, what = "statement"), "\\s*X\\s*-+>\\s*Y\\s*")
})






test_that("inspect outputs correct nodes", {
  expect_output(inspect(model, what = "nodes"), "Nodes:\\s*X, Y")
})

test_that("inspect outputs correct parents_df", {
  expect_output(
    inspect(model, what = "parents_df"),
    "Root vs Non-Root status with number and names of parents for each node:"
  )
  expect_output(inspect(model, what = "parents_df"),
                "node\\s+root\\s+parents\\s+parent_nodes")
  expect_output(inspect(model, what = "parents_df"), "1\\s+X\\s+TRUE\\s+0")
  expect_output(inspect(model, what = "parents_df"),
                "2\\s+Y\\s+FALSE\\s+1\\s+X")
})

test_that("inspect outputs correct parameters_df", {
  expect_output(inspect(model, what = "parameters_df"),
                "Mapping of model parameters to nodal types:")
  expect_output(
    inspect(model, what = "parameters_df"),
    "param_names\\s+node\\s+gen\\s+param_set\\s+nodal_type\\s+given\\s+param_value\\s+priors"
  )
  expect_output(
    inspect(model, what = "parameters_df"),
    "X\\.0\\s+X\\s+1\\s+X\\s+0\\s+0\\.50\\s+1"
  )
  expect_output(
    inspect(model, what = "parameters_df"),
    "Y\\.11\\s+Y\\s+2\\s+Y\\s+11\\s+0\\.25\\s+1"
  )
})

test_that("inspect outputs correct causal_types", {
  expect_output(inspect(model, what = "causal_types"), "Causal Types:")
  expect_output(inspect(model, what = "causal_types"), "X0\\.Y00\\s+0\\s+00")
  expect_output(inspect(model, what = "causal_types"), "X1\\.Y11\\s+1\\s+11")
})

test_that("inspect outputs correct prior_distribution", {
  expect_output(
    inspect(model, what = "prior_distribution"),
    "Summary statistics of model parameters prior distributions:"
  )
  expect_output(
    inspect(model, what = "prior_distribution"),
    "Distributions matrix dimensions are\\s+4000 rows \\(draws\\) by 6 cols \\(parameters\\)"
  )
})

test_that("inspect throws error for posterior_distribution", {
  expect_error(
    inspect(model, what = "posterior_distribution"),
    "Model does not contain: posterior_distribution"
  )
})


model <- update_model(
  model,
  data = data,
  keep_fit = TRUE,
  keep_event_probabilities = TRUE
)

test_that("inspect outputs correct posterior_distribution", {
  expect_output(
    inspect(model, what = "posterior_distribution"),
    "Summary statistics of model parameters posterior distributions:"
  )
})

test_that("inspect outputs correct posterior_event_probabilities", {
  expect_output(
    inspect(model, what = "posterior_event_probabilities"),
    "Posterior draws of event probabilities \\(transformed parameters\\):"
  )
})

test_that("inspect outputs correct type_distribution", {
  expect_output(
    inspect(model, what = "type_distribution"),
    "Posterior draws of causal types \\(transformed parameters\\):"
  )
})

test_that("inspect outputs correct data", {
  expect_output(inspect(model, what = "data"), "Data used to update the model:")
})

test_that("inspect outputs correct stanfit", {
  expect_output(inspect(model, what = "stanfit"), "Inference for Stan model: simplexes.")
})



test_that("inspect handles dots", {
  model <- make_model("X->Y")

  expect_true(model |>
                inspect("prior_hyperparameters", nodes = "Y") |>
                length() == 4)

  expect_true(
    grab(model, what = "data_types", complete_data = TRUE) |>
      nrow() == 4)

  expect_true(
    grab(model, what = "data_types", complete_data = FALSE) |>
      nrow() == 9)

})



