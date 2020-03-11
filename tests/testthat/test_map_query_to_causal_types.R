context("Testing map_query_to_causal_type")

testthat::test_that(
	desc = "print statement",
	code = {
		model <- make_model("X -> Y")
		out <- capture.output(gbiqq:::print.summary.causal_types(model$causal_types))
		expect_equal(length(out), 9)
	}
)

testthat::test_that(
	desc = "print statement",
	code = {
		model <- make_model("X->Y") %>% set_restrictions(labels = list(Y ="01"))
		out <- map_query_to_causal_type(model, query = "X==1")
		expect_output(print(out), "X1.Y00")
		expect_error(expect_output(print(out), "X1.Y01"))
	}
)
