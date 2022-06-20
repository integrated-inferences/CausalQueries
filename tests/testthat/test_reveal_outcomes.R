




context(desc = "Testing realise_outcomes")

testthat::skip_on_cran()
model <- make_model("X->M->Y")

testthat::test_that(

	desc = "error when dos is not parent",

	code = {
		expect_error(realise_outcomes(model, dos = list(X = 1), node = "Y"))
	}
)

testthat::test_that(

	desc = "return when node stated",

	code = {
		expect_equal(nrow(realise_outcomes(model, dos = list(X = 1), node = "M")), 4)
	}
)


test_that("reveal_outcomes is deprecated", {

    expect_warning(
      CausalQueries:::reveal_outcomes(model, dos = list(X = 1), node = "M")
    )
  }
})
