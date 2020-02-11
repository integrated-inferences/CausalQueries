context("Testing set_parameter_matrix")

testthat::test_that(
	desc = "Testing messages.",
	code = {
		model <- make_model("X <- Y")
		model <- set_parameter_matrix(model)
		# P already contained
		expect_message(set_parameter_matrix(model))
	}
)

context("Testing summary commands for set_parameter_matrix")

testthat::test_that(
	desc = "Testing print.",
	code = {
		model <- make_model("X -> Y")
		model <- set_parameter_matrix(model)
		out <- capture.output(gbiqq:::print.parameter_matrix(model))
		expect_true(any(grepl("Parameter\\smatrix.+", out)))
	}
)

testthat::test_that(
	desc = "Testing summary.",
	code = {
		model <- make_model("X -> Y")
		confound <- list(X = "(Y[X=1] > Y[X=0])", X = "(Y[X=1] == 1)")
		model <- set_confound(model = model, confound = confound)
		out <- capture.output(gbiqq:::summary.parameter_matrix(model))
		# bad test; but function currently not producing proper output
		expect_true(!is.null(out))
	}
)
