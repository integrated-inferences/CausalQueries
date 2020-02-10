context("Testing get_ambiguity_matrix")

testthat::test_that(
	desc = "Test if null.",
	code = {
		model <- make_model("X -> Y")
		model <- set_ambiguities_matrix(model)
		expect_true(!is.null(model$A))
	}
)
