
context(desc = "Testing data_strategy")

model <- make_model("X -> M -> Y")

testthat::test_that(
	desc = "data strategy works",
	code = {
		strat <- data_strategy(model, n_obs = 8)
    expect_equal(nrow(strat), 8)

    strat <- data_strategy(
			    	   model,
			    	   n_obs = 8,
			    	   n = NULL,
			         vars = list(c("X", "Y"), "M"),
			    	   probs = list(1, .5),
			    	   subsets = list(NULL, "X==1 & Y==0"))
		# Test subset functionality
	  subsetM <- strat[!is.na(strat$M), ]
	  expect_equal(subsetM$X, rep(1, nrow(subsetM)))
	  expect_equal(subsetM$Y, rep(0, nrow(subsetM)))
})

testthat::test_that(
	desc = "data_strategy errors and messages when it should",
	code = {
		no_p <- make_model("X -> M -> Y", add_parameters = FALSE)

     # "If specified, vars, probs, subsets, should have the same length"
		expect_error(data_strategy(model, vars = c("X","M","Y"), probs = c(1,2), subsets = c("X==1", "Y==1", "X==0")))
		#"If specified, n should be the same length as vars"
		expect_error(data_strategy(model, vars = c("X","M","Y"), n = 1))
		#"parameters not provided"
		expect_message(data_strategy(no_p, n_obs = 8))

	}
)
