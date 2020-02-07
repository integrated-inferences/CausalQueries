context("Error messages of get_types")

testthat::test_that(
	desc = "Proper error messages.",
	code = {
		model <- make_model("X -> M -> Y; X->Y")
		query <- c("(Y[X = .]==1)", "(Y[X = 0] == 0)")
		expect_error(get_types(model, query), "Please specify a query of length 1L.")
		query <- "(Y[Z = .]==1)"
		expect_error(get_types(model, query), "Variable Z is not part of the model.")
		query <- "(Y[Z = . == 1)"
		expect_error(get_types(model, query), "Either '[' or ']' missing.")

	}
)

context("Proper output.")

testthat::test_that(
	desc = "Proper output.",
	code = {
		model <- make_model("X -> M -> Y; X->Y")
		query <- "(Y[X = .]==1)"
		out <- get_types(model, query)
		expect_equal(out$query, "(Y[X=0]==1 | Y[X=1]==1)")
		expect_equal(ncol(out$evaluated_nodes), 5)
		expect_equal(nrow(out$evaluated_nodes), 128)
		expect_equal(length(out$type_list), 96)
		expect_equal(out$manipulated_outcomes, "Y")
		query <- "(Y == 1)"
		out <- get_types(model, query)
		expect_equal(nrow(out$evaluated_nodes), 128)
	}
)

context("Summary functions of get_types")

testthat::test_that(
	desc = "Check the summary functions of get_types",
	code = {
		model <- make_model("X -> M -> Y; X->Y")
		query <- "(Y[X = .]==1)"
		out <- get_types(model, query)
		expect_output(print(out))
		summary_out <- class(summary(out))
		expect_equal(summary_out[1], "summary.causal_types")
		expect_equal(summary_out[2], "data.frame")
		out$types <- as.numeric(out$types)
		expect_output(print(out))
	}
)
