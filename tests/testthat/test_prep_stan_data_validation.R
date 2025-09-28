test_that("prep_stan_data succeeds on valid model/data", {
  model <- make_model("X -> Y")
  data_long <- make_data(model, n = 10)
  data_compact <- collapse_data(data_long, model)

  expect_no_error({
    sd <- CausalQueries:::prep_stan_data(model, data_compact)
    expect_true(is.list(sd))
  })
})


