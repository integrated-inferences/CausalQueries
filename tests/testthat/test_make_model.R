context("Testing make_model")

testthat::skip_on_cran()

testthat::test_that(

	desc = "Print and summary functions",

	code = {
		model <- make_model("X -> Y")
		out <- capture.output(print(model))
		expect_true(any(grepl("X -> Y", out)) &
		              any(grepl("^Number of nodal types by node:", out)) &
		              any(grepl("^Number of causal types:", out)))
		out <- class(summary(model))
		expect_equal(out, "summary.causal_model")

		model <- update_model(model)
		out <- capture.output(print(model))
		expect_no_warning(print(summary(model), stanfit = TRUE))
		expect_true(any(grepl("Model has been updated.+", out)))

		model <- make_model("X -> Y") |> set_confound(list("X <-> Y"))
		model <- make_model("X->Y") |> set_restrictions(statement = c("X[] == 0"))
		out <- capture.output(print(summary(model)))
		expect_true(any(grepl("Restrictions.+", out)))
 	}
)



testthat::test_that(

  desc = "Check errors",

  code = {
    expect_error(make_model("X -> S <- Y; S <-> Z"))
    expect_error(make_model(c("X -> Y" ,  "M -> Y")))
    expect_error(make_model(1))
    expect_error(make_model("X ->"))
    expect_error(make_model("X -> -> Y"))
    expect_error(make_model("institutions -> political_inequality"))
    expect_error(make_model("political-inequality <- institutions"))
    expect_error(make_model("institutions -> political>inequality"))
    expect_error(make_model("institutions -> political<inequality"))
    expect_error(make_model("X.exp( -> Y"))
    expect_error(make_model("X.log( -> Y"))
    expect_error(make_model("X^Y -> Y"))
    expect_error(make_model("X/Y -> Y"))
    expect_error(make_model("X[Y] -> Y"))
    expect_error(make_model("X:|:Y -> Y"))
    expect_error(make_model("X -> Y; Y -> X"))
    expect_error(make_model("X -> M; M -> W; W -> X; Z -> Y"))
  }
)


testthat::test_that(

  desc = "Nodal types",

  code = {
    expect_error(make_model("X -> Y" , nodal_types = list(Z = c("0", "1"))))
    expect_error(make_model("X -> Y" , nodal_types = list(Y = c("0", "1"))))
    expect_message(make_model("X -> Y" ,
                              nodal_types = list(
                                Y = c("00", "01", "10", "11"),
                                X = c("0", "1"))
                              ))
    expect_message(make_model("X -> Y" , nodal_types = FALSE))
    expect_message(make_model("Z -> Y",
                              nodal_types = list(
                                Y = c("01", "10"),
                                Z = c("0", "1"))
                              ))
  }
)



testthat::test_that(

  desc = "Clean statement",

  code = {
    expect_equal(make_model("X -> Y<-  X") |> grab("statement"), "X -> Y")
    expect_equal(make_model("X -> Y; X<->Y; Y<->X") |> grab("statement"), "X -> Y; X <-> Y")
  }
)

