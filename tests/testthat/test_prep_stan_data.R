




context("Testing prep_stan_data")

testthat::skip_on_cran()
testthat::test_that(

	desc = "prep_stan_data errors",

	code = {
		model <- make_model('X->Y')
		data  <-  collapse_data(make_data(model, n = 6), model)
		expect_error(prep_stan_data(model, data[, 1:2]))
	}
)


testthat::test_that(

  desc = "prep_stan_data errors and data consistency",

  code = {
    expect_error(prep_stan_data(make_model(), data.frame(event = "A", count = 0)))

    expect_error(prep_stan_data(make_model(), data.frame(event = "X0", count = 0)), NA)

    expect_equal(prep_stan_data(make_model(), data.frame(event = "X0", count = 1))$data |> dim(),
                 c(2,3))

    expect_equal(prep_stan_data(make_model(), data.frame(event = "X0Y1", count = 1))$data |> dim(),
                 c(4,3))

    expect_equal(prep_stan_data(make_model(), data.frame(event = c("X0", "X0Y1"), count = 1:2))$data |> dim(),
                 c(6,3))

      }
)

