
context(desc = "Testing make_data 1")

testthat::skip_on_cran()
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

  desc = "Simulate data works when probs supplied without n_steps.",

  code = {
    dat <- make_data(model, n = 8, probs = 0.5)
    expect_equal(nrow(dat), 8)
    expect_equal(ncol(dat), 2)

    # probs = 1 still short circuits to the complete data
    dat <- make_data(model, n = 8, probs = 1)
    expect_false(any(is.na(dat)))
  }
)

testthat::test_that(

  desc = "Non-negative integer number of observations; n = 0 is empty data.",

  code = {
    expect_error(make_data(model, n = -1),
                 "Number of observations has to be a non-negative integer.")
    expect_error(make_data(model, n = 1.5),
                 "Number of observations has to be a non-negative integer.")

    # compact empty data: one row per event type, all counts zero
    events0 <- make_events(model, n = 0)
    expect_equal(sum(events0$count), 0L)

    # long-form empty data: zero rows, correct columns
    dat0 <- make_data(model, n = 0)
    expect_equal(nrow(dat0), 0L)
    expect_equal(names(dat0), model$nodes)
  }
)

context("Testing make_data 2")

testthat::skip_on_cran()
testthat::test_that(

	desc = "make_data_single",

	code = {
		model <- make_model("X -> Y") |>
			set_priors(alphas = c(1, 1, 1, 0, 0 , 0))
		out <- colSums(make_data_single(model, n = 1e3, param_type = "prior_draw"))
		expect_true(out[1] > out[2])
		model <- make_model("X -> Y") |>
			set_parameters(parameters = c(1, 0, 1, 1, 0, 0))
		out <- colSums(make_data_single(model, n = 1e3))
		expect_true(out[2] > out[1])
	}
)

testthat::test_that(

	desc = "observe_data",

	code = {
		model <- make_model("X -> Y")
		df <- make_data(model, n = 8)
		out <- observe_data(complete_data = df,
     observed = observe_data(complete_data = df,
                             nodes_to_observe = c("X", "Y")),
     nodes_to_observe = "X",
     prob = 1,
     subset = "X==1 | X == 0")
		expect_true(all(c(out$X, out$Y)))
	}
)



