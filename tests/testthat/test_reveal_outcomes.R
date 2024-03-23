




context(desc = "Testing realise_outcomes")

testthat::skip_on_cran()
model <- make_model("X->M->Y")

testthat::test_that(

	desc = "error when dos is not parent",

	code = {
		expect_error(realise_outcomes(model,
		                              dos = list(X = 1),
		                              node = "Y"))

	  expect_warning(realise_outcomes( make_model("X-> M -> Y; Z->Y"),
	                    dos = list(X = 1, M = 1),
	                    node = "Y"))

	}
)

testthat::test_that(

	desc = "return when node stated",

	code = {
		expect_equal(nrow(realise_outcomes(model,
		                                   dos = list(X = 1),
		                                   node = "M")), 4)
	}
)


test_that(
  desc = "reveal_outcomes is deprecated",

  code = {
    model <- make_model("X->M->Y")
    expect_warning(CausalQueries:::reveal_outcomes(model,
                                                   dos = list(X = 1),
                                                   node = "M"))
  }
)

test_that(
  desc = "realise_outcomes with single node",

  code = {
    out <-  realise_outcomes(model =  make_model("X"), add_rownames = TRUE)
    expect_true(all_equal(out, data.frame(X = c("0","1"), row.names = c(0,1))))
  }
)



