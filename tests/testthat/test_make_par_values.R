




context("Test make_par_values")

testthat::skip_on_cran()
testthat::test_that(

	desc = "Missing check.",

	code = {
		model <- make_model("X->Y")
		expect_equal(capture_messages(CausalQueries:::make_par_values(model, y = c(), x = "X.0", normalize = TRUE)), "No change to values\n")
	}
)

