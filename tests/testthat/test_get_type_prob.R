context("Testing get_type_prob")

testthat::test_that(

	desc = "get_type_prob",
	code = {
		expect_equal(length(get_type_prob(model = make_model('X->Y'))), 8)
		expect_true(is.numeric(get_type_prob(model = make_model('X->Y'))))

	desc = "Test function works just the same with arg parameters",
	code = {
		model <- make_model("X -> Y")
		expect_identical(get_type_prob(model), get_type_prob(model, parameters = rep(1, 6)))
		model <- make_model('X -> Y') %>%
			set_confound(list('X <-> Y'))
		expect_identical(get_type_prob(model), get_type_prob(model, parameters = rep(1, 10)))

	}
)

testthat::test_that(

	desc = "get_param_dist",
	code = {
		expect_error(get_param_dist(model = make_model('X->Y'), using = 'posteriors', n_draws = 4))
	}
)


	desc = "Test error if model has not been updated",
	code = {
		model <- make_model("X -> Y")
		expect_true(is.null(model$posterior_distribution))
		expect_error(get_param_dist(model, using="posteriors"))
		data_long   <- simulate_data(model, n = 4)
		model <- update_model(model, data_long)
		expect_true(!is.null(model$posterior_distribution))
	}
)



