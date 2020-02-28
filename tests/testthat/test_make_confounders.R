context("Testing make_confounds")


testthat::test_that(
	desc = "Check messages.",
	code = {
		model <- make_model("X -> Y")
		expect_message(make_confounds_df(model))
		expect_message(model <- set_confound(model, list(X = 'X==1')))
	}
)

