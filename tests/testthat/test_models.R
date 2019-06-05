
context(desc = "Testing that canonical models work as they should")

dags <- c("X -> Y", "X -> M -> Y", "X -> Y; Z -> Y")

for(dag in dags){

	testthat::test_that(
		desc = "Model returns a non-null object.",
		code = expect_true(length(make_model(dag)) > 1)
	)

	testthat::test_that(
		desc = "Model returns the correct object class.",
		code = expect_identical(class(make_model(dag)), "probabilistic_causal_model")
	)

}

testthat::test_that(
	desc = "Test functions on model X -> Y without confounding",
	code = {

		XY_noconf <- make_model("X -> Y")
		expect_equal(dim(get_parameter_matrix(XY_noconf)), c(6,8))
		data <- simulate_data(XY_noconf, n = 1, parameters = c(.5, .5, .2, .4, .2, .2))
		expect_equal(dim(data), c(1,2))

		updated <- posterior <- gbiqq(XY_noconf, data, refresh = 0)
		expect_true(!is.null(posterior))

		ATE <- "Y[X=1] - Y[X=0]"
		COE <- "Y[X=1] > Y[X=0]"
		results <- get_estimands(
			updated,
			queries = list(ATE = ATE, ATE = ATE, COE = COE, COE = COE),
			using = list("priors", "posteriors"))
		expect_true(is.data.frame(results))
	}
)


testthat::test_that(
	desc = "Test functions on model X -> Y with confounding",
	code = {
		XY_conf <- make_model("X -> Y")
		XY_conf <- set_confound(XY_conf,
														list(X = "(Y[X=1]>Y[X=0])",
																 X = "(Y[X=1]<Y[X=0])",
																 X = "(Y[X=1] ==1)"))

		expect_equal(dim(get_parameter_matrix(XY_conf)), c(12,8))

		parameters <- c(.5, .5, .5, .5, .5, .5, .5, .5, .1, .7, .1, .1)
		data <- simulate_data(XY_conf, n = 1, parameters = parameters)
		expect_equal(c(1, 2), dim(data))

		posterior <- gbiqq(XY_conf, data, refresh = 0)
		expect_true(!is.null(posterior))

		prior_ate <- estimand_distribution(posterior, "c(Y[X=1] - Y[X=0])", using = "priors")
		post_ate <- estimand_distribution(posterior, "c(Y[X=1] - Y[X=0])", using = "posteriors")

		results <- get_estimands(
			posterior,
			queries = list(COE = "c(Y[X=1] > Y[X=0])"),
			subsets = c("X==1 & Y==1"),
			using = "posteriors")

		expect_true(is.data.frame(results))
	}
)


testthat::test_that(
	desc = "Test functions on mediator model (X -> M -> Y)",
	code = {
		XY_mediator <- make_model("X -> M -> Y")

		data <- simulate_data(
			XY_mediator, n = 1,
			parameters = c(.5, .5, .2, .8, 0, 0, 0, .8, 0, .2))
		expect_equal(c(1, 3), dim(data))

		posterior <- gbiqq(XY_mediator, data, refresh = 0)
		expect_true(!is.null(posterior))

		results <- get_estimands(
			posterior,
			queries = list(COE = "c(Y[X=1] > Y[X=0])"),
			subsets = c("X==1 & Y==1", "X==1 & Y==1 & M==0", "X==1 & Y==1 & M==1"),
			using = "posteriors")

		expect_true(is.data.frame(results))
	}
)

testthat::test_that(
	desc = "Test functions on moderator model (X -> Y; Z -> Y)",
	code = {
		XY_moderator <- make_model("X -> Y; Z -> Y")

		data <- simulate_data(
			XY_moderator, n = 1,
			parameters = c(.5, .5, .5, .5,
										 .02, .02, .02, .02, .02, .02, .02, .02,
										 .02, .70, .02, .02, .02, .02, .02, .02))
		expect_equal(c(1, 3), dim(data))

		posterior <- gbiqq(XY_moderator, data, refresh = 0)
		expect_true(!is.null(posterior))

		results <- get_estimands(
			posterior,
			queries = list(COE = "c(Y[X=1] > Y[X=0])"),
			subsets = c("X==1 & Y==1", "X==1 & Y==1 & Z==0", "X==1 & Y==1 & Z==1"),
			using = "posteriors")

		expect_true(is.data.frame(results))
	}
)
