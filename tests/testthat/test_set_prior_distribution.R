context("Set_prior_distribution")

testthat::skip_on_cran()
testthat::test_that(

	desc = "Check messages.",

	code = {
		expect_equal(make_model("X -> Y") |>
		               set_prior_distribution(30) |>
		                 inspect("prior_distribution") |>
		               dim(), c(30, 6))
	}
)



testthat::skip_on_cran()
testthat::test_that(

  desc = "Check priors distribution for one node models.",

  code = {
    expect_equal(make_model("X") |>
                   set_prior_distribution(n_draws = 10) |>
                   inspect("prior_distribution") |>
                   ncol(), 2)
  }
)

testthat::skip_on_cran()
testthat::test_that(

  desc = "Deprecated get_prior_distribution.",

  code = {
    expect_warning(make_model("X -> Y") |>
                   CausalQueries:::get_prior_distribution(n_draws = 10) |>
                   ncol())
  }
)



