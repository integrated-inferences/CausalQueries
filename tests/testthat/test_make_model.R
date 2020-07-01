
.runThisTest <- Sys.getenv("RunAllRcppTests") == "yes"

if (.runThisTest) {

context("Make_models")

testthat::test_that(
testthat::skip_on_cran()
	desc = "Print and summary functions",
	code = {
		model <- make_model("X -> Y")
		out <- capture.output(CausalQueries:::print.causal_model(model))
		expect_true(any(grepl("\\$X", out)) & any(grepl("\\$Y", out)))
		out <- class(CausalQueries:::summary.causal_model(model))
		expect_equal(out[1], "summary.causal_model")
		expect_equal(out[2], "data.frame")
		model <- make_model("X -> Y") %>% set_confound(list("X <-> Y"))
		out <- capture.output(CausalQueries:::print.summary.causal_model(model))
		expect_true(any(grepl("Parameter matrix.+", out)))
		model <- make_model("X->Y") %>% set_restrictions(statement = c("X[] == 0"))
		out <- capture.output(CausalQueries:::print.summary.causal_model(model))
		expect_true(any(grepl("Restrictions.+", out)))
		expect_error(make_model("X -> S <- Y; S <-> Z"))
	}
)


}
