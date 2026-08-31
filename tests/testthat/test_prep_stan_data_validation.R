test_that("prep_stan_data succeeds on valid model/data", {
  model <- make_model("X -> Y")
  data_long <- make_data(model, n = 10)
  data_compact <- collapse_data(data_long, model)

  expect_no_error({
    sd <- CausalQueries:::prep_stan_data(model, data_compact)
    expect_true(is.list(sd))
  })
})


test_that("prep_stan_data rejects non-contiguous parameters_df blocks", {
  model <- make_model("X -> Y")
  data_compact <- collapse_data(make_data(model, n = 10), model)

  # simplex boundaries are derived from cumulative counts and are only correct
  # if each param_set and node occupies a single run of rows
  scrambled <- model
  scrambled$parameters_df <-
    model$parameters_df[order(model$parameters_df$nodal_type), ]

  expect_error(CausalQueries:::prep_stan_data(scrambled, data_compact),
               "not contiguous")
})


test_that("prep_stan_data validates censored_types", {
  model <- make_model("X -> Y")
  data_compact <- collapse_data(make_data(model, n = 10), model)

  expect_error(CausalQueries:::prep_stan_data(model, data_compact,
                                              censored_types = "NOT_A_TYPE"),
               "Unrecognized")

  # censoring a type that is present drops it
  expect_equal(
    CausalQueries:::prep_stan_data(model, data_compact,
                                   censored_types = "X1Y0")$n_events,
    CausalQueries:::prep_stan_data(model, data_compact)$n_events - 1)

  # a valid data type of the model that happens to be absent from this data
  # is accepted: censoring may be why it is absent
  expect_no_error(CausalQueries:::prep_stan_data(model, data_compact,
                                                 censored_types = "X0"))
})


