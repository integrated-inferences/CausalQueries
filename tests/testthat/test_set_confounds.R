




context("Testing set_confounds")

testthat::skip_on_cran()
testthat::test_that(

	desc = "Test alias is working ",

	code = {
		model <- make_model('X -> Y') %>%
			set_confound(list('X <-> Y'))
		models <- make_model('X -> Y') %>%
			set_confounds(list('X <-> Y'))
		expect_identical(model, models)
	}
)


testthat::test_that(

  desc = "set_confound warns when prob of types is greater than 1",

  code = {
    model <- make_model("X -> Y <- M")
    expect_warning(set_confound(model, confound = list(M = "Y[X=1]==1")))
  }
)
