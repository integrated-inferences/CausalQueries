

context("Test make_par_values")

testthat::test_that(
	desc = "Missing check.",
	code = {
		model <- make_model("X->Y")
		expect_error((CausalQueries:::make_par_values(model, x = "x0")))
		expect_equal(capture_messages(CausalQueries:::make_par_values(model, y = c(), x = 0,  normalize = TRUE)), "No change to values\n")
	}
)

