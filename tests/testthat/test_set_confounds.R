




context("Testing set_confounds")

testthat::test_that(
testthat::skip_on_cran()
	desc = "Test alias is working ",
	code = {
		model <- make_model('X -> Y') %>%
			set_confound(list('X <-> Y'))
		models <- make_model('X -> Y') %>%
			set_confounds(list('X <-> Y'))
		expect_identical(model, models)
	}
)

