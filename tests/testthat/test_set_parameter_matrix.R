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
		expect_output(gbiqq:::print.parameter_matrix(model))
	}
)

testthat::test_that(
	desc = "Testing summary.",
	code = {
		model <- make_model("X -> Y")
		confound <- list(X = "(Y[X=1] > Y[X=0])", X = "(Y[X=1] == 1)")
		model <- set_confound(model = model, confound = confound)
		expect_output(gbiqq:::summary.parameter_matrix(model))
	}
)
