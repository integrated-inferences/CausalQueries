
.runThisTest <- Sys.getenv("RunAllRcppTests") == "yes"

if (.runThisTest) {

context("Testing make_confounds")

testthat::test_that(
	desc = "Check messages.",
	code = {
		model <- make_model("X -> Y")
		expect_message(make_confounds_df(model))
		expect_warning(model <- model %>%
									 	set_confound(list(X = 'X==1')))
	}
)

testthat::test_that(
	desc = "Set confounds",
	code = {
		## passer function
		model_1 <- make_model('X -> Y') %>%
			set_confound(list(Y = 'X'))
		model_2 <- make_model('X -> Y') %>%
			set_confounds(list(Y = 'X'))
		expect_identical(model_1, model_2)
	}
)


}
