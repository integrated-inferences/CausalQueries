




context("Testing map_query_to_causal_type")

testthat::skip_on_cran()
testthat::test_that(

	desc = "correct mapping",

	code = {
		model <- make_model("X -> Y")
		map <- map_query_to_causal_type(model, "Y[X=1] > Y[X=0]")

		causal_types <- c("X0.Y00", "X1.Y00",
		                  "X0.Y10", "X1.Y10",
		                  "X0.Y01", "X1.Y01",
		                  "X0.Y11", "X1.Y11")

		implicated <- c("X0.Y01", "X1.Y01")
		expected_map <- causal_types %in% implicated
		names(expected_map) <- causal_types

		expect_equal(map$type_list, implicated)
		expect_equal(map$types, expected_map)
	}
)

