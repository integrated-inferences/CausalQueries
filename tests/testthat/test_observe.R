





context(desc = "Testing observe")

testthat::skip_on_cran()
set.seed(1)
model <- make_model("X -> Y")
df  <- make_data(model, n = 4)


testthat::test_that(

	desc = "observe works when only complete_data is specified",

	code = {

		obs <- observe_data(complete_data = df)
    expect_equal(nrow(obs), 4)
    expect_true(all((colnames(obs) ==  model$nodes)))
}
)

testthat::test_that(

	desc = "observe works when nodes_to_observe is specified",

	code = {
		obs <- observe_data(complete_data = df, nodes_to_observe = "X")
		expect_true(all(obs$X))
		expect_true(all(!obs$Y))
	}
)


testthat::test_that(

	desc = "observe works when observed is specified",

	code = {

		obs1 <- observe_data(complete_data = df, nodes_to_observe = "X")
		obs2 <- observe_data(complete_data = df,
										 observed = obs1,
										 nodes_to_observe = "Y")
		expect_true(all(obs2$X))
		expect_true(all(obs2$Y))

	}
)

# testthat::test_that(

# 	desc = "observe works when subset is specified",

# 	code = {
#
# 		obs1 <-   observe_data(complete_data = df, nodes_to_observe = "X")
# 		obs2 <- 	observe_data(complete_data = df,
# 										  observed = obs1,
# 										  nodes_to_observe = "Y",
# 										  subset = "X==1")
#
# 		expect_true(all(obs2$X))
#
#
# 	}
# )

testthat::test_that(

	desc = "subsets referring to unobserved nodes do not error",

	code = {
		# "Y==1" evaluates to NA for every row because Y has not been observed
		expect_message(
			obs <- observe_data(complete_data = df,
			                    nodes_to_observe = "X",
			                    subset = "X==1 & Y==1"),
			"Empty subset"
		)
		expect_true(all(!obs$X))
		expect_true(all(!obs$Y))
	}
)

testthat::test_that(

	desc = "a single candidate row is the row that gets observed",

	code = {
		# only row 7 satisfies X==1; sample(7, 1) would draw from 1:7
		single <- data.frame(X = c(rep(0, 6), 1), Y = rep(0, 7))
		obs_x <- observe_data(complete_data = single, nodes_to_observe = "X")

		picked <- replicate(50, which(observe_data(complete_data = single,
		                                           observed = obs_x,
		                                           nodes_to_observe = "Y",
		                                           m = 1,
		                                           subset = "X==1")$Y))
		expect_equal(unique(picked), 7L)
	}
)

testthat::test_that(

	desc = "asking for more units than the subset holds errors informatively",

	code = {
		expect_error(
			observe_data(complete_data = df, nodes_to_observe = "X", m = 10),
			"Cannot observe 10 units"
		)
	}
)

testthat::test_that(

	desc = "m overrides p (observe)",

	code = {
	  obs <- observe_data(
	    complete_data = df,
	    observed = observe_data(complete_data = df, nodes_to_observe = "X"),
	    nodes_to_observe = "Y",
	    prob = 1,
	    m = 2,
	    subset = "X==1"
	  )

		expect_equal(sum(obs$Y), 2)
	}
)




