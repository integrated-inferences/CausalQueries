
context("causal_model summary method check; see inspect tests for more")

testthat::skip_on_cran()
testthat::test_that(

	desc = "Proper summaries.",

	code = {
	   model <-
	     make_model('X -> Y')

	   expect_output(
	     print(summary(model), what = "statement"),
	     "\\s*X\\s*-+>\\s*Y\\s*")


	}

)






