
set.seed(1)
model <- make_model("X -> Y")
df  <- simulate_data(model, n = 4)


testthat::test_that(
	desc = "observe works when only complete_data is specified",
	code = {

		obs <- observe(complete_data = df)
    expect_equal(nrow(obs), 4)
    expect_equal(colnames(obs), model$variables)
}
)

testthat::test_that(
	desc = "observe works when vars_to_observe is specified",
	code = {
		obs <- observe(complete_data = df, vars_to_observe = "X")
		expect_true(all(obs$X))
		expect_true(all(!obs$Y))
	}
)


testthat::test_that(
	desc = "observe works when observed is specified",
	code = {

		obs1 <- observe(complete_data = df, vars_to_observe = "X")
		obs2 <- 	observe(complete_data = df,
										 observed = obs1,
										 vars_to_observe = "Y")
		expect_true(all(obs2$X))
		expect_true(all(obs2$Y))

	}
)

testthat::test_that(
	desc = "observe works when subset is specified",
	code = {

		obs1 <-   observe(complete_data = df, vars_to_observe = "X")
		obs2 <- 	observe(complete_data = df,
										  observed = obs1,
										  vars_to_observe = "Y",
										  subset = "X==1")

		expect_true(all(obs2$X))
		expect_equal(sum(is.na(obs2$Y)), 5)

	}
)

testthat::test_that(
	desc = "m overrides p",
	code = {

		obs <- observe(complete_data    = df,
								  	observed        = observe(complete_data = df, vars_to_observe = "X"),
									  vars_to_observe = "Y",
										prob            = 1,
										m               = 2,
										subset          = "X==1")


		expect_equal(sum(obs$Y), 2)

	}
)




