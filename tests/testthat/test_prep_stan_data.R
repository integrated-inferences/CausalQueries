




context("Testing prep_qbiqq_data")

testthat::test_that(

	desc = "prep_stan_data errors",
testthat::skip_on_cran()
	code = {
		model <- make_model('X->Y')
		data  <-  collapse_data(simulate_data(model, n = 6), model)
		expect_error(prep_stan_data(model, data[, 1:2]))
	}
)

