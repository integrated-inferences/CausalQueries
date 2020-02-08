context("Set_prior_distribution")

testthat::test_that(
	desc = "Check messages.",
	code = {
		expect_message(make_model("X -> Y") %>% get_prior_distribution(3))
		expect_equal(make_model("X -> Y") %>% set_prior_distribution(n_draws = 10) %>% get_prior_distribution() %>% nrow(), 10)
	}
)
