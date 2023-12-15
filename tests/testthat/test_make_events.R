

context(desc = "Testing make_events.")

testthat::skip_on_cran()
testthat::test_that(

	desc = "User input",

	code = {
		softmax <- function(x) return(exp(x)/sum(exp(x)))
		model <- make_model("X -> Y")
		expect_silent(make_events(model = model, parameters = NULL))
		w <- matrix(softmax(rnorm(4)), nrow = 4, ncol = 1)
		rownames(w) <- c("A", "B", "C", "D")
		expect_equal(as.character(make_events(model = model, w = w)[, 1]),
		             rownames(w))
		w[, 1] <- c(1, 0, 0, 0)
		expect_equal(make_events(model = model, w = w, n = 10)[1, 2], 10)
		expect_equal(colnames(make_events(model = model,
		                                  include_strategy = TRUE))[2], "strategy")

		out <- make_events(model = model, include_strategy = TRUE,
		                   clean_params = TRUE, parameters = c(1, 0, 0, 0, 1, 0))
		expect_equal(out$count[1], 1)
	}
)



testthat::test_that(

	desc = "param_type input test.",

	code = {
		model <- make_model("X -> Y")
		expect_is(make_events(model = model, param_type = "flat"),
		          "data.frame")
		expect_is(make_events(model = model, param_type = "prior_mean"),
		          "data.frame")
		error_message <- "Posterior distribution required"
		expect_error(make_events(model = model, param_type = "posterior_mean"),
		             error_message)
		expect_error(make_events(model = model, param_type = "posterior_draw"),
		             error_message)
		expect_is(make_events(model = model, param_type = "prior_mean"),
		          "data.frame")
	}
)

testthat::test_that(

	desc = "Check output.",

	code = {
		model <- make_model("X -> Y")
		n <- rpois(1, 55) + 1
		out <- make_events(model = model, n = n)
		expect_equal(sum(out[, 2]), n)
	}
)

testthat::test_that(

	desc = "Check errors.",

	code = {
		softmax <- function(x) return(exp(x)/sum(exp(x)))
		model <- make_model("X -> Y")
		w <- matrix(softmax(rnorm(4)), nrow = 4, ncol = 1)
		expect_error(make_events(model = model, w = w),
		             "w has to be a matrix with named rows.")
		w <- c(w)
		expect_error(make_events(model = model, w = w), "w has to be a matrix.")
		expect_error(
		  make_events(model = model, param_type = "define"),
		  "neither distribution nor values provided. No change to values")
	}
)

