
.runThisTest <- Sys.getenv("RunAllRcppTests") == "yes"

if (.runThisTest) {

context("Error messages of map_query_to_causal_type")

testthat::test_that(
testthat::skip_on_cran()
	desc = "Proper error messages.",
	code = {
		model <- make_model("X -> M -> Y; X->Y")
		query <- c("(Y[X = .]==1)", "(Y[X = 0] == 0)")
		expect_error(map_query_to_causal_type(model, query), "Please specify a query of length 1L.")
		query <- "(Y[Z = .]==1)"
		expect_error(map_query_to_causal_type(model, query), "Variable Z is not part of the model.")
		query <- "(Y[Z = . == 1)"
		expect_error(map_query_to_causal_type(model, query),"Either '[' or ']' missing.", fixed = TRUE)
		query <- "(Y[] == 1)"
		expect_error(map_query_to_causal_type(model, query))
	}
)

context("Proper output.")

testthat::test_that(
testthat::skip_on_cran()
	desc = "Proper output.",
	code = {
		model <- make_model("X -> M -> Y; X->Y")
		query <- "(Y[X = .]==1)"
		out <- map_query_to_causal_type(model, query)
		expect_equal(out$query, "(Y[X=0]==1 | Y[X=1]==1)")
		expect_equal(ncol(out$evaluated_nodes), 5)
		expect_equal(nrow(out$evaluated_nodes), 128)
		expect_equal(length(out$type_list), 96)
		query <- "(Y == 1)"
		out <- map_query_to_causal_type(model, query)
		expect_equal(nrow(out$evaluated_nodes), 128)
	}
)

context("Summary functions of map_query_to_causal_type")

testthat::test_that(
testthat::skip_on_cran()
	desc = "Check the summary functions of map_query_to_causal_type",
	code = {
		model <- make_model("X -> M -> Y; X->Y")
		query <- "(Y[X = .]==1)"
		out <- map_query_to_causal_type(model, query)
		expect_output(print(out))
		summary_out <- class(summary(out))
		expect_equal(summary_out[1], "summary.causal_types")
		expect_equal(summary_out[2], "data.frame")
		out$types <- as.numeric(out$types)
		expect_output(print(out))
	}
)
}
