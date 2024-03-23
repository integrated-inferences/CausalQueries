




context("Testing summary commands for set_parameter_matrix")

testthat::skip_on_cran()
testthat::test_that(

	desc = "Testing print.",

	code = {
		model <- make_model("X -> Y")
		model <- set_parameter_matrix(model)
		out <- capture.output(CausalQueries:::print.parameter_matrix(model$P))
		expect_true(any(grepl(" parameter set", out)))
	}
)

testthat::test_that(

	desc = "Testing summary.",

	code = {
		model <- make_model("X -> Y")
		confound <- list(X = "(Y[X=1] > Y[X=0])", X = "(Y[X=1] == 1)")
		model <- set_confound(model = model, confound = confound)
		out <- capture.output(print(model$P))
		expect_true(out[[2]] == "Rows are parameters, grouped in parameter sets")
	}
)



testthat::test_that(

  desc = "Read P.",

  code = {
    model <- make_model("X -> Y")
    model <- set_parameter_matrix(model, P = get_parameter_matrix(model))
    expect_true(!is.null(model$P))
  }
)


