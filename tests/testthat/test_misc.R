




context("Testing misc")

testthat::test_that(

	desc = "sampling_args user_adapt_delta",
testthat::skip_on_cran()
	code = {
		object <- NULL
		user_dots = list(control = list(user_adapt_delta = 10))
		out <- CausalQueries:::set_sampling_args(object = object, user_dots = user_dots, user_adapt_delta = 10)
		expect_equal(out$control$adapt_delta, 10)
	}
)


