context("Testing get_nodal_types")


testthat::test_that(
	desc = "Testing collapse_nodal_types",
	code = {
		model <- make_model("X -> Y")
		n_types <- get_nodal_types(model)
		expect_mapequal(n_types, list(X = c("0", "1"), Y = c("00","10", "01", "11")))
	}
)



