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
	desc = "If the matrix is there, return it",
	code = {
		model <- make_model("X -> Y")
		model <- set_ambiguities_matrix(model)
		A <- get_ambiguities_matrix(model)
		expect_is(A, "ambiguities_matrix")
		expect_equal(ncol(A), 4)
		expect_equal(nrow(A), 8)
		model2 <-  make_model("X -> Y")
		B <- make_ambiguities_matrix(model2)
		expect_setequal(as.vector(A), as.vector(B))
	}
)
