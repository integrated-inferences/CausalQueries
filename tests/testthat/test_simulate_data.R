
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

context(desc = "Testing simulate_events.")

testthat::test_that(
	desc = "User input",
	code = {
		softmax <- function(x) return(exp(x)/sum(exp(x)))
		model <- make_model("X -> Y")
		expect_silent(simulate_events(model = model, parameters = NULL))
		w <- c(softmax(gtools::rdirichlet(4, 1)))
	}
)

textthat::test_that(
	desc = "Check output.",
	code = {
		model <- make_model("X -> Y")
		n <- rpois(1, 55) + 1
		out <- simulate_events(model = model, n = n)
		expect_equal(sum(out[, 2]), n)
	}
)
