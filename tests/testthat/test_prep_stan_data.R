
.runThisTest <- Sys.getenv("RunAllRcppTests") == "yes"

if (.runThisTest) {

context("Testing prep_qbiqq_data")

testthat::test_that(
	desc = "prep_stan_data errors",
	code = {
		model <- make_model('X->Y')
		data  <-  collapse_data(simulate_data(model, n = 6), model)
		expect_error(prep_stan_data(model, data[, 1:2]))
	}
)
}
