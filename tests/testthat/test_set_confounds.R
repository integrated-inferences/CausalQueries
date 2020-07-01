




context("Testing set_confounds")

testthat::test_that(

	desc = "Test alias is working ",
testthat::skip_on_cran()
	code = {
		model <- make_model('X -> Y') %>%
			set_confound(list('X <-> Y'))
		models <- make_model('X -> Y') %>%
			set_confounds(list('X <-> Y'))
		expect_identical(model, models)
	}
)

