
context(desc = "Testing make_data")

model <- make_model("X -> M -> Y")

testthat::test_that(
	desc = "data strategy works",
	code = {
		strat <- make_data(model, n = 8)
    expect_equal(nrow(strat), 8)
    set.seed(1)
    strat <- make_data(
			    	   model,
			    	   n = 20,
			         vars = list(c("X", "Y"), "M"),
			    	   probs = list(1, .5),
			    	   subsets = list(NULL, "X==1 & Y==0"))
		# Test subset functionality
	  subsetM <- strat[!is.na(strat$M), ]
	  expect_equal(subsetM$X, rep(1, nrow(subsetM)))
	  expect_equal(subsetM$Y, rep(0, nrow(subsetM)))


})

testthat::test_that(
	desc = "make_data errors and messages when it should",
	code = {
		model <- make_model("X -> M -> Y")

     # "If specified, vars, probs, subsets, should have the same length"
		expect_error(make_data(model, vars = c("X","Y"), probs = c(1,2), subsets = c("X==1", "Y==1", "X==0")))
		#"If specified, n_steps should be the same length as vars"
		expect_error(make_data(model, vars = list("X","M","Y"), n_steps = 1))

		expect_warning(
			make_data(
			model,
			n = 8,
			n_steps = c(2,2),
			vars = list(c("X", "Y"), "M"),
			probs = list(1, .5),
			subsets = list(NULL, "X==1 & Y==0")))

	}
)

