model <- make_model("X -> M -> Y")




testthat::test_that(
	desc = "data strategy works",
	code = {
		strat <- data_strategy(model, n_obs = 8)
    expect_equal(nrow(strat), 8)
	}
)

testthat::test_that(
	desc = "data_strategy errors when it should",
	code = {

		expect_error(data_strategy(model, n = c(4,5,6), probs = c(1,2), subsets = c("X==1", "Y==1", "X==0")))

		expect_error(data_strategy(model, vars = "X",probs = 0.5, subsets = n = c(1,2)))
	}
)
#' data_strategy(
#'   model,
#'   n_obs = 8,
#'   n = NULL,
#'   vars = list(c("X", "Y"), "M"),
#'   probs = list(1, .5),
#'   subsets = list(NULL, "X==1 & Y==0"))
