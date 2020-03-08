context("Testing get_type_prob")

testthat::test_that(
	desc = "get_type_prob",
	code = {
		expect_equal(length(get_type_prob(model = make_model('X->Y'))), 8)
		expect_true(is.numeric(get_type_prob(model = make_model('X->Y'))))
	}
)

testthat::test_that(
	desc = "get_param_dist",
	code = {
		expect_error(get_param_dist(model = make_model('X->Y'), using = 'posteriors', n_draws = 4))
	}
)

