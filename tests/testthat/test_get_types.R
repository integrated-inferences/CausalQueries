context("Error messages of get_types")

testthat::test_that(
	desc = "Proper error messages.",
	code = {
		model <- make_model("X -> M -> Y; X->Y")
		query <- c("(Y[X = .]==1)", "(Y[X = 0] == 0)")
		expect_error(get_types(model, query), "Please specify a query of length 1L.")
	}
)
