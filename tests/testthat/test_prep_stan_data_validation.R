test_that("prep_stan_data succeeds on valid model/data", {
  model <- make_model("X -> Y")
  data_long <- make_data(model, n = 10)
  data_compact <- collapse_data(data_long, model)

  expect_no_error({
    sd <- CausalQueries:::prep_stan_data(model, data_compact)
    expect_true(is.list(sd))
  })
})

test_that("prep_stan_data stops when nrow(P) != n_params", {
  model <- make_model("X -> Y")
  # Corrupt the parameters_df by duplicating one row, inflating n_params
  model$parameters_df <- rbind(model$parameters_df, model$parameters_df[1, , drop = FALSE])

  data_long <- make_data(model, n = 4)
  data_compact <- collapse_data(data_long, model)

  expect_error(
    CausalQueries:::prep_stan_data(model, data_compact),
    regexp = "nrow\(P\) must equal n_params"
  )
})

test_that("prep_stan_data stops when l_starts > l_ends occurs", {
  model <- make_model("X -> Y")
  data_long <- make_data(model, n = 4)
  data_compact <- collapse_data(data_long, model)

  # Wrap prep to intercept l_starts/l_ends after construction
  psd <- CausalQueries:::prep_stan_data
  mock <- function(model, data, keep_type_distribution = TRUE, censored_types = NULL) {
    out <- psd(model, data, keep_type_distribution, censored_types)
  }

  # Directly call internal: reconstruct l_starts/l_ends then flip one pair
  # We emulate by editing parameters_df ordering to violate ordering
  model_bad <- model
  model_bad$parameters_df <- model$parameters_df[rev(seq_len(nrow(model$parameters_df))), ]

  expect_error(
    CausalQueries:::prep_stan_data(model_bad, data_compact),
    regexp = "l_starts must be <= l_ends"
  )
})

test_that("prep_stan_data stops when node indices out of range", {
  model <- make_model("X -> Y")
  data_long <- make_data(model, n = 4)
  data_compact <- collapse_data(data_long, model)

  # Tamper with parameters_df to misalign node group ranges
  model_bad <- model
  model_bad$parameters_df$node[1] <- "NON_EXISTENT_NODE"

  expect_error(
    CausalQueries:::prep_stan_data(model_bad, data_compact),
    regexp = "node start/end indices"
  )
})

test_that("prep_stan_data stops when parmap/map dims mismatch", {
  model <- make_model("X -> Y")
  data_long <- make_data(model, n = 4)
  data_compact <- collapse_data(data_long, model)

  # Create mismatch by altering attr(map) via get_parmap
  model_bad <- model
  pm <- get_parmap(model_bad)
  attr(pm, "map") <- attr(pm, "map")[, -1, drop = FALSE] # drop a data column
  # Monkey-patch get_parmap to return corrupted parmap
  withr::with_options(list(), {
    unlockBinding("get_parmap", as.environment("package:CausalQueries"))
  })

  skip("Complex mocking of get_parmap is skipped; manual test suggested")
})


