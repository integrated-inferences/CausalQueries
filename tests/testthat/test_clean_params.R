
context(desc = "Testing clean_params")

testthat::test_that(
	desc = "negative alphas",
	code = {
		model <- make_model('X -> Y')
		model$parameters_df$priors <- rep(-1, 6)
		expect_error(clean_params(model$parameters_df))
	}
)

testthat::test_that(
  desc = "negative parameters",
  code = {
    model <- make_model('X -> Y')
    model$parameters_df$param_value <- rep(-1, 6)
    expect_error(CausalQueries:::clean_params(model$parameters_df))
  }
)

testthat::test_that(
	desc = "normalized params warning",
	code = {
		model <- make_model('X -> Y')
		model$parameters_df$param_value <- c(.70, .80, .25, .25, .25, .25)
		expect_message(clean_params(model$parameters_df))
	}
)


testthat::test_that(
  desc = "empty paramaters_df",
  code = {
    model <- make_model('X -> Y')
    model$parameters_df <- model$parameters_df |> filter(node == "A")
    expect_message(clean_params(model$parameters_df))
  }
)

testthat::test_that(
  desc = "check atomic inputs",
  code = {
    model <- make_model('X -> Y')
    expect_error(CausalQueries:::clean_param_vector(model, list(1)))
  }
)

