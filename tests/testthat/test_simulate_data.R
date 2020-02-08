
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
		model <- make_model("X -> Y")
		expect_silent(simulate_events(model = model, parameters = NULL))
		w <- softmax(gtools::rdirichlet(4, 1))
		rownames(w) <- c("A", "B", "C", "D")
		expect_equal(as.character(simulate_events(model = model, w = w)[, 1]), rownames(w))
		w[, 1] <- c(1, 0, 0, 0)
		expect_equal(simulate_events(model = model, w = w, n = 10)[1, 2], 10)
		expect_equal(colnames(simulate_events(model = model, include_strategy = TRUE))[2], "strategy")
	}
)

testthat::test_that(
	desc = "param_type input test.",
	code = {
		model <- make_model("X -> Y")
		expect_output(simulate_events(model = model, param_type = "flat"))
		expect_output(simulate_events(model = model, param_type = "prior_mean"))
		error_message <- "Posterior distribution required"
		expect_error(simulate_events(model = model, param_type = "posterior_mean"), error_message)
		expect_error(simulate_events(model = model, param_type = "posterior_draw"), error_message)
		expect_is(simulate_events(model = model, param_type = "define", distribution = "jeffreys"), "data.frame")
	}
)
# ("flat", "prior_mean", "posterior_mean", "prior_draw", "posterior_draw", "define)
textthat::test_that(
	desc = "Check output.",
	code = {
		model <- make_model("X -> Y")
		n <- rpois(1, 55) + 1
		out <- simulate_events(model = model, n = n)
		expect_equal(sum(out[, 2]), n)
	}
)

testthat::test_that(
	desc = "Check errors.",
	code = {
		softmax <- function(x) return(exp(x)/sum(exp(x)))
		model <- make_model("X -> Y")
		w <- matrix(softmax(rnorm(4)), nrow = 4, ncol = 1)
		expect_error(simulate_events(model = model, w = w), "w has to be a matrix with named rows.")
		w <- c(w)
		expect_error(simulate_events(model = model, w = w), "w has to be a matrix.")
	}
)

testthat::test_that(
	desc = "Check warnings.",
	code = {
		model <- make_model("X -> Y")
		expect_warning(simulate_events(model = model, param_type = "define"), "neither distribution nor alphas provided; no change to priors")
	}
)

