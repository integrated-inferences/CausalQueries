
context(desc = "Testing make_data")


# Simulate using parameters
model <- make_model("X -> Y")

testthat::test_that(
	desc = "simulate data works using parameter",
	code = {
		dat <- make_data(model, n = 5)
		expect_equal(nrow(dat), 5)

	})

testthat::test_that(
	desc = "simulate data works using priors",
	code = {
		dat <- make_data(model, n = 5, param_type = "prior_draw")
		expect_equal(nrow(dat), 5)
	})


