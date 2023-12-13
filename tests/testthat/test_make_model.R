context("Testing make_model")

testthat::skip_on_cran()

testthat::test_that(

	desc = "Print and summary functions",

	code = {
		model <- make_model("X -> Y")
		out <- capture.output(CausalQueries:::print.causal_model(model))
		expect_true(any(grepl("\\$X", out)) & any(grepl("\\$Y", out)))
		out <- class(CausalQueries:::summary.causal_model(model))
		expect_equal(out[1], "summary.causal_model")
		expect_equal(out[2], "data.frame")
		model <- make_model("X -> Y") |> set_confound(list("X <-> Y"))
		model <- make_model("X->Y") |> set_restrictions(statement = c("X[] == 0"))
		out <- capture.output(CausalQueries:::print.summary.causal_model(model))
		expect_true(any(grepl("Restrictions.+", out)))
 	}
)


testthat::test_that(

  desc = "Check errors",

  code = {
    expect_error(make_model("X -> S <- Y; S <-> Z"))
    expect_error(make_model(c("X -> Y" ,  "M -> Y")))
    expect_error(make_model(1))
    expect_error(make_model("institutions -> political_inequality"))
    expect_error(make_model("political-inequality <- institutions"))
    expect_error(make_model("X -> Y; Y -> X"))
    expect_error(make_model("X -> M; M -> W; W -> X; Z -> Y"))
  }
)


testthat::test_that(

  desc = "Nodal types",

  code = {
    expect_error(make_model("X -> Y" , nodal_types = list(Z = c("0", "1"))))
    expect_message(make_model("X -> Y" , nodal_types = list(Y = c("0", "1"))))
    expect_message(make_model("X -> Y" , nodal_types = FALSE))
    expect_message(
      make_model("Z -> Y", nodal_types = list(Y = c("01", "10"),
                                              Z = c("0", "1"))))
  }
)

