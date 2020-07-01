




context("Testing get_type_prob")

testthat::skip_on_cran()
testthat::test_that(



	desc = "get_type_prob",
	code = {
		expect_equal(length(get_type_prob(model = make_model('X->Y'))), 8)
		expect_true(is.numeric(get_type_prob(model = make_model('X->Y'))))
		}
)

testthat::test_that(

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






