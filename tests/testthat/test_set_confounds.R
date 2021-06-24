




context("Testing set_confounds")

testthat::skip_on_cran()
testthat::test_that(

	desc = "Test alias is working ",

	code = {
		model <- make_model('X -> Y') %>%
			set_confound(list('Y <-> X'))
		models <- make_model('X -> Y') %>%
			set_confounds(list('X <-> Y'))
		expect_identical(model, models)
	}
)


