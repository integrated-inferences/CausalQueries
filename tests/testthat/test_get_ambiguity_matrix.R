context("Testing get_ambiguity_matrix")

testthat::test_that(
	desc = "Test if null.",
	code = {
		model <- make_model("X -> Y")
		model <- set_ambiguities_matrix(model)
		expect_true(!is.null(model$A))
	}
)

testthat::test_that(
	desc = "Return if not null.",
	code = {
		model <- make_model("X -> Y")
		model1 <- set_ambiguities_matrix(model)
		A1    <- get_ambiguities_matrix(model1)[]
		A2    <- get_ambiguities_matrix(model)
		expect_identical(A1, A2)
	}
)
