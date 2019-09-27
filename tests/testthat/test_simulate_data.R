
context(desc = "Testing data_strategy")


# Simulate using parameters
model <- make_model("X -> Y")

testthat::test_that(
	desc = "simulate data works using parameter",
	code = {
		dat <- simulate_data(model, n = 5)
		expect_equal(nrow(dat), 5)

	})

testthat::test_that(
	desc = "simulate data works using priors",
	code = {
		dat <- simulate_data(model, n = 5, using = "priors")
		expect_equal(nrow(dat), 5)

	})


