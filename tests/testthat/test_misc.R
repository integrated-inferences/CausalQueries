




context("Testing misc")

testthat::skip_on_cran()
testthat::test_that(

	desc = "sampling_args uses defaults when no control supplied",

	code = {
		out <- CausalQueries:::set_sampling_args(object = NULL,
		                                         user_dots = list())
		expect_equal(out$control$adapt_delta, 0.95)
		expect_equal(out$control$max_treedepth, 15L)
		expect_false(out$save_warmup)
	}
)

testthat::test_that(

	desc = "sampling_args respects user supplied control arguments",

	code = {
		out <- CausalQueries:::set_sampling_args(
			object = NULL,
			user_dots = list(control = list(adapt_delta = 0.99)))
		expect_equal(out$control$adapt_delta, 0.99)
		# unspecified control elements still get defaults
		expect_equal(out$control$max_treedepth, 15L)

		out <- CausalQueries:::set_sampling_args(
			object = NULL,
			user_dots = list(control = list(max_treedepth = 20L)))
		expect_equal(out$control$max_treedepth, 20L)
		expect_equal(out$control$adapt_delta, 0.95)
	}
)

testthat::test_that(

	desc = "sampling_args respects user supplied save_warmup",

	code = {
		out <- CausalQueries:::set_sampling_args(object = NULL,
		                                         user_dots = list(save_warmup = TRUE))
		expect_true(out$save_warmup)
	}
)


