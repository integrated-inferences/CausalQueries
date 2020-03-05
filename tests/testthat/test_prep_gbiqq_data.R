context("Testing prep_qbiqq_data")

testthat::test_that(
	desc = "prep_gbiqq_data errors",
	code = {
		model <- make_model('X->Y')
		data  <-  collapse_data(simulate_data(model, n = 6), model)
		expect_error(prep_gbiqq_data(model, data[, 1:2]))
	}
)
