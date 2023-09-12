




context("Tests make_data")

testthat::skip_on_cran()
testthat::test_that(

	desc = "make_data_single",

	code = {
		model <- make_model("X -> Y") %>%
			set_priors(alphas = c(1, 1, 1, 0, 0 , 0))
		out <- colSums(make_data_single(model, n = 1e3, param_type = "prior_draw"))
		expect_true(out[1] > out[2])
		model <- make_model("X -> Y") %>%
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





