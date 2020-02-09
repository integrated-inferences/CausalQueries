context("Testing set_priors")

testthat::test_that(
	desc = "Check warnings",
	code = {
		model <- make_model("X -> Y")
		expect_warning(make_priors(model, label = c("X0", "Y1"), alphas = c(1)))
		expect_warning(make_priors(model, statement = c("Y[M=0] > Y[M=1]"), alphas = c(NA)))
		model <- make_model("X -> Y")
		make_priors(model, label = c("X0", "Y1"),
								distribution = c("jeffreys"))
	}
)

testthat::test_that(
	desc = "Check errors.",
	code = {
		model <- make_model("X -> M -> Y")
		expect_error(make_priors(model, statement = "Y[X=1] > Y[X=0]", alphas = 3))
		expect_error(make_priors(model, statement = c("Y[M=0] > Y[M=1]"), alphas = c(-1)))
	}
)



