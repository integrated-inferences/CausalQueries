
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
	desc = "normalized params warning",
	code = {
		model <- make_model('X -> Y')
		model$parameters_df$param_value <- c(.70, .80, .25, .25, .25, .25)
		expect_message(clean_params(model$parameters_df))
	}
)
