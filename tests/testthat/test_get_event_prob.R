context("Testing get_event_prob")

testthat::test_that(
	desc = "Testing warnings",
	code = {
		model <- make_model("X -> Y")
		expect_error(get_event_prob(model = model, parameters = rnorm(6)),"Parameters cannot take on negative values")
	}
)
