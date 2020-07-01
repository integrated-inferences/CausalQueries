




context(desc = "Testing make_model")

testthat::skip_on_cran()
testthat::test_that(

	desc = "Observational equivalence corresponds to model equivalence.",

	code = {
		m1 <- make_model("X -> Y -> Z <- U")
		m2 <- make_model("U -> Z <- Y <- X")
		expect_equal(m1$dag, m2$dag)
		expect_equal(m1$nodes, m2$nodes)
		expect_equal(m1$nodal_types, m2$nodal_types)
		expect_equal(m1$parameters_df, m2$parameters_df)
		expect_equal(m1$causal_types, m2$causal_types)
	}
)



#################################################
context(desc = "Testing make_data")

testthat::skip_on_cran()
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
			         nodes = list(c("X", "Y"), "M"),
			    	   probs = list(1, .5),
			    	   subsets = list(NULL, "X==1 & Y==0"))
		# Test subset functionality
	  subsetM <- strat[!is.na(strat$M), ]
	  expect_equal(subsetM$X, rep(1, nrow(subsetM)))
	  expect_equal(subsetM$Y, rep(0, nrow(subsetM)))


})

testthat::test_that(

	desc = "make_data errors and messages when it should.",

	code = {
		model <- make_model("X -> M -> Y")

     # "If specified, vars, probs, subsets, should have the same length"
		expect_error(make_data(model, nodes  = c("X","Y"), probs = c(1,2), subsets = c("X==1", "Y==1", "X==0")))
		#"If specified, n_steps should be the same length as vars"
		expect_error(make_data(model, nodes = list("X","M","Y"), n_steps = 1))

		expect_warning(
			make_data(
			model,
			n = 8,
			n_steps = c(2,2),
			nodes = list(c("X", "Y"), "M"),
			probs = list(1, .5),
			subsets = list(NULL, "X==1 & Y==0")))

	}
)


