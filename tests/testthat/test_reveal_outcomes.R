




context(desc = "Testing reveal_outcomes")

model <- make_model("X->M->Y")

testthat::test_that(
testthat::skip_on_cran()
	desc = "error when dos is not parent",
	code = {
		expect_error(reveal_outcomes(model, dos = list(X = 1), node = "Y"))
	}
)

testthat::test_that(
testthat::skip_on_cran()
	desc = "return when node stated",
	code = {
		expect_equal(nrow(reveal_outcomes(model, dos = list(X = 1), node = "M")), 4)
	}
)


