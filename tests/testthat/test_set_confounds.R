




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


testthat::test_that(

  desc = "Adding confounds to model statememt",

  code = {
    model <- make_model('X -> Y -> W') %>%
      set_confound(list('Y <-> X', 'X<->W'))

    expect_identical(model$statement, "X -> Y -> W ; Y <-> X ; W <-> X")
  }
)


testthat::test_that(

  desc = "Test for confound errors",

  code = {
    expect_error(
      make_model('X -> Y -> W; X <-> Y') %>%
      set_confound(list('Y <-> X', 'X<->W'))
    )

    expect_error(
      make_model('X -> Y -> W') %>%
        set_confound(list('Y <-> X; X<->W'))
    )
  }
)

