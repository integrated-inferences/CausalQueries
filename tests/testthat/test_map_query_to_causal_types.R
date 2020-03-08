context("Testing map_query_to_causal_type")

testthat::test_that(
	desc = "print statement",
	code = {
		model <- make_model("X -> Y")
		out <- capture.output(gbiqq:::print.summary.causal_types(model$causal_types))
		expect_equal(length(out), 9)
	}
)
