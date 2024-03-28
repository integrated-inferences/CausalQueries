





context("Set_prior_distribution")

testthat::skip_on_cran()
testthat::test_that(

	desc = "Check messages.",

	code = {
		expect_message(make_model("X -> Y") |>
		                 grab("prior_distribution", 3))
		expect_equal(make_model("X -> Y") |>
		               set_prior_distribution(n_draws = 10) |>
		               grab("prior_distribution") |> nrow(), 10)
	}
)



testthat::skip_on_cran()
testthat::test_that(

  desc = "Check priors distribution for one node models.",

  code = {
    expect_equal(make_model("X") |>
                   set_prior_distribution(n_draws = 10) |>
                   grab("prior_distribution") |>
                   ncol(), 2)
  }
)

