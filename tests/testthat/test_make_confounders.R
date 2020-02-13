context("Testing make_confounders")





testthat::test_that(
	desc = "Check messages.",
	code = {
		model <- make_model("X -> Y")
		expect_message(make_confounds_df(model))
	}
)
