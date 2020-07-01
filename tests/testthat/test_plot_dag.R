




context(desc = "Testing plot_dag")

testthat::test_that(
testthat::skip_on_cran()
	desc = "Testing plot.dag",
	code = {
		model <- make_model("X -> M -> Y; X -> Y")
		pdf(file = NULL)
		expect_silent(plot(model))
		model <- make_model("X -> M -> Y")  %>%
			set_confound(confound = list(M = "Y[M=1]==1"))
		model$confounds_df <- NULL
		expect_that(plot_dag(model), shows_message())
	}
)

testthat::test_that(
testthat::skip_on_cran()
	desc = "Testing translate_daggity",
	code = {
		model <- make_model("X")
		expect_equal(translate_dagitty(model), "dag{ X }")
		model <- make_model("X -> M -> Y")  %>%
			set_confound(confound = list(M = "Y[M=1]==1"))
		expect_equal(translate_dagitty(model), "dag{ X -> M ; M -> Y  ;  M <-> Y }")

	}
)

testthat::test_that(
testthat::skip_on_cran()
	desc = "Testing warning",
	code = {
		model <- make_model('X -> K -> Y; X -> Y')
		model <- set_parameter_matrix(model)
		expect_message(plot_dag(model))
	}
)

