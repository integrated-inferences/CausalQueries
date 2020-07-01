




context(desc = "Testing reveal_outcomes")

model <- make_model("X->M->Y")

testthat::test_that(

	desc = "error when dos is not parent",
testthat::skip_on_cran()
	code = {
		expect_error(reveal_outcomes(model, dos = list(X = 1), node = "Y"))
	}
)

testthat::test_that(

	desc = "return when node stated",
testthat::skip_on_cran()
	code = {
		expect_equal(nrow(reveal_outcomes(model, dos = list(X = 1), node = "M")), 4)
	}
)


