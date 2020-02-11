context("Make_date")

testthat::test_that(
	desc = "Print and summary functions",
	code = {
		model <- make_model("X -> Y")
		out <- capture.output(gbiqq:::print.causal_model(model))
		expect_true(any(grepl("\\$X", out)) & any(grepl("\\$Y", out)))
		out <- class(gbiqq:::summary.causal_model(model))
		expect_equal(out[1], "summary.causal_model")
		expect_equal(out[2], "data.frame")
		model <- make_model("X -> Y") %>% set_confound(list("X <-> Y"))
		out <- capture.output(gbiqq:::print.summary.causal_model(model))
		expect_true(any(grepl("Parameter matrix.+", out)))
		model <- make_model("X->Y") %>% set_restrictions(statement = c("X == 0"))
		out <- capture.output(gbiqq:::print.summary.causal_model(model))
		expect_true(any(grepl("Restrictions.+", out)))
	}
)


