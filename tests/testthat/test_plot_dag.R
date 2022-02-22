




context(desc = "Testing plot_dag")

testthat::skip_on_cran()
testthat::test_that(

	desc = "Testing plot.dag",

	code = {
		model <- make_model("X -> M -> Y; X -> Y")
		pdf(file = NULL)
		expect_silent(plot(model))
		model <- make_model("X -> M -> Y; M <-> Y")
		model$confounds_df <- NULL
		expect_that(plot_dag(model), shows_message())
	}
)

testthat::test_that(

	desc = "Testing translate_daggity",

	code = {
		model <- make_model("X")
		expect_equal(translate_dagitty(model), "dag{ X }")
		model <- make_model("X -> M -> Y; M <-> Y")
		expect_equal(translate_dagitty(model), "dag{ X -> M ; M -> Y  ;  Y <-> M }")

	}
)

testthat::test_that(

	desc = "Testing warning",

	code = {
		model <- make_model('X -> K -> Y; X -> Y')
		model <- set_parameter_matrix(model)
		expect_message(plot_dag(model))
	}
)

