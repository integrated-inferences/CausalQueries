


context("Testing get_posterior_distribution")

test_that("get_posterior_distribution triggers deprecation warning", {
  model <- make_model("X -> Y") |> update_model()
  expect_warning(CausalQueries:::get_posterior_distribution(model = model),
                 regexp = "is deprecated")
})
