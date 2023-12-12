




context("Testing get_type_prob")

testthat::skip_on_cran()

testthat::test_that(
  desc = "get_type_prob",
  code = {
    expect_equal(length(get_type_prob(model = make_model('X->Y'))), 8)
    expect_true(is.numeric(get_type_prob(model = make_model('X->Y'))))
    })

testthat::test_that(

	desc = "Test function works just the same with arg parameters",

	code = {
		model <- make_model("X -> Y")
		expect_identical(get_type_prob(model),
		                 get_type_prob(model, parameters = rep(1, 6)))

		model <- set_confound(model, list('X <-> Y'))
		expect_identical(get_type_prob(model),
		                 get_type_prob(model, parameters = rep(1, 10)))

	}
)

testthat::test_that(

  desc = "get_type_prob_multiple",
  code = {
    expect_equal(
      dim(suppressMessages(get_type_prob_multiple(make_model('X->Y'),
                                                  using = 'priors',
                                                  n_draws = 3))),
      c(8,3)
    )

    expect_message(
      get_type_prob_multiple(make_model('X->Y'),
                             using = 'priors',
                             n_draws = 3),
      "Prior distribution added to model"
    )

    expect_equal(
      length(get_type_prob_multiple(make_model('X->Y'),
                             using = 'parameters')),
      8
    )

  }
)

testthat::test_that(

	desc = "get_param_dist",
	code = {
		expect_error(
		  get_param_dist(model = make_model('X->Y'),
		                 using = 'posteriors',
		                 n_draws = 4),
		  "Model does not contain a posterior distribution"
		)

	  expect_equal(
	    make_model("X -> Y") |>
	      update_model(data.frame(X = 0, Y = 0),
	                   refresh = 0, chains = 1, iter = 10, warmup = 5) |>
	      suppressWarnings() |>
	      get_param_dist(model = _,
	                     using = 'posteriors',
	                     n_draws = 4) |>
	      dim(),
	    c(5, 6)
	  )

	  expect_type(
	    get_param_dist(model = make_model('X->Y'),
	                   using = 'parameters',
	                   n_draws = 4),
	    "double"
	  )

	  expect_message(
	    get_param_dist(model = make_model('X->Y'),
	                   using = 'priors',
	                   n_draws = 4),
	    "Prior distribution added to model"
	  )

	  expect_type(
	    suppressMessages(
	      get_param_dist(model = make_model('X->Y'),
	                     using = 'priors',
	                     n_draws = 4)),
	    "list"
	  )
	}
)


