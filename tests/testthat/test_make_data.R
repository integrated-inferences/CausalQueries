context("Tests make_data")

testthat::test_that(
	desc = "make_data_single",
	code = {
		model <- make_model("X -> Y") %>%
			set_priors(priors = c(1, 1, 1, 0, 0 , 0))
		out <- colSums(make_data_single(model, n = 1e3, param_type = "prior_draw"))
		expect_true(out[1] > out[2])
		model <- make_model("X -> Y") %>%
			set_parameters(parameters = c(1, 0, 1, 1, 0, 0))
		out <- colSums(make_data_single(model, n = 1e3))
		expect_true(out[2] > out[1])
	}
)
