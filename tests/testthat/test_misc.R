context("Testing misc")

testthat::test_that(
	desc = "sampling_args user_adapt_delta",
	code = {
		object <- NULL
		user_dots <- NULL
		user_adapt_delta = 10
		out <- gbiqq:::set_sampling_args(object = object, user_dots = user_dots, user_adapt_delta = user_adapt_delta)
		expect_equal(out$control$adapt_delta, user_adapt_delta)
	}
)

