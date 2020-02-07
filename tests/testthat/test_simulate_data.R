
context(desc = "Testing make_data")


# Simulate using parameters
model <- make_model("X -> Y")

testthat::test_that(
	desc = "Simulate data works using parameter.",
	code = {
		dat <- make_data(model, n = 5)
		expect_equal(nrow(dat), 5)

	})

testthat::test_that(
	desc = "Simulate data works using priors.",
	code = {
		dat <- make_data(model, n = 5, param_type = "prior_draw")
		expect_equal(nrow(dat), 5)
	})

testthat::test_that(
	desc = "Positive integer number of observations.",
	code = {
		expect_error(make_data(model, n = 0), "Number of observation has to be an integer bigger than 0.")
	}
)
