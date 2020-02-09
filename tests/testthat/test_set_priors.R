context("Testing set_priors")

testthat::test_that(
	desc = "",
	code = {
		model <- make_model("X -> Y")
		expect_warning(make_priors(model, label = c("X0", "Y1"), alphas = c(1)))
		model <- make_model("X -> Y")
		make_priors(model, label = c("X0", "Y1"),
								distribution = c("jeffreys"))
	}
)
