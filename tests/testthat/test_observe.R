





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
    expect_equal(colnames(obs), model$nodes)
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

	desc = "m overrides p (observe)",

	code = {
		obs <- observe_data(complete_data    = df,
								  	observed        = observe_data(complete_data = df, nodes_to_observe = "X"),
									  nodes_to_observe = "Y",
										prob            = 1,
										m               = 2,
										subset          = "X==1")

		expect_equal(sum(obs$Y), 2)
	}
)




