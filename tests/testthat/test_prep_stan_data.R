




context("Testing prep_stan_data")

testthat::skip_on_cran()
testthat::test_that(

	desc = "prep_stan_data errors",

	code = {
		model <- make_model('X->Y')
		data  <-  collapse_data(make_data(model, n = 6), model)
		expect_error(prep_stan_data(model, data[, 1:2]))
	}
)

