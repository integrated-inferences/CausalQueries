





context(desc = "Testing that canonical models work as they should")

testthat::skip_on_cran()
dags <- c("X -> Y", "X -> M -> Y", "X -> Y; Z -> Y")

for(dag in dags){

	testthat::test_that(

		desc = "Model returns a non-null object.",

		code = expect_true(length(make_model(dag)) > 1)
	)

	testthat::test_that(

		desc = "Model returns the correct object class.",

		code = expect_identical(class(make_model(dag)), "causal_model")
	)

}

testthat::test_that(

	desc = "Test functions on model X -> Y without confounding",

	code = {

		XY_noconf <- make_model("X -> Y")
		expect_equal(dim(get_parameter_matrix(XY_noconf)), c(6,8))
		data <- simulate_data(XY_noconf, n = 1, parameters = c(.5, .5, .2, .4, .2, .2))
		expect_equal(dim(data), c(1,2))

		updated <- posterior <- update_model(XY_noconf, data, refresh = 0)
		expect_true(!is.null(posterior))

		ATE <- "Y[X=1] - Y[X=0]"
		COE <- "Y[X=1] > Y[X=0]"
		results <- CausalQueries::query_model(
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
																 X = "(Y[X=1]==1) & (Y[X=0]==1)"))

		expect_equal(dim(get_parameter_matrix(XY_conf)), c(12,8))

		parameters <- c(.5, .5, .5, .5, .5, .5, .5, .5, .1, .7, .1, .1)
		data <- simulate_data(XY_conf, n = 1, parameters = parameters)
		expect_equal(c(1, 2), dim(data))


		posterior <- update_model(XY_conf, data, refresh = 0, keep_transformed = TRUE)
		expect_true(!is.null(posterior))

		posterior <- update_model(XY_conf, data, refresh = 0)
		expect_true(!is.null(posterior))

		prior_ate <- query_distribution(model = posterior,
																			 query = "(Y[X=1] - Y[X=0])",
																			 using = "priors")
		post_ate <- query_distribution(posterior, "c(Y[X=1] - Y[X=0])", using = "posteriors")

		results <- query_model(
			posterior,
			queries = list(COE = "c(Y[X=1] > Y[X=0])"),
			given = c("X==1 & Y==1"),
			using = "posteriors")

		expect_true(is.data.frame(results))

		expect_message(set_confound(XY_conf))
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

		posterior <- update_model(XY_mediator, data, refresh = 0)
		expect_true(!is.null(posterior))

		expect_equal(nrow(simulate_data(posterior , n = 5, using = "posteriors")), 5)

		results <- query_model(
			posterior,
			queries = list(COE = "c(Y[X=1] > Y[X=0])"),
			given = c("X==1 & Y==1", "X==1 & Y==1 & M==0", "X==1 & Y==1 & M==1"),
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

		posterior <- update_model(XY_moderator, data, refresh = 0)
		expect_true(!is.null(posterior))

		results <- query_model(
			posterior,
			queries = list(COE = "c(Y[X=1] > Y[X=0])"),
			given = c("X==1 & Y==1", "X==1 & Y==1 & Z==0", "X==1 & Y==1 & Z==1"),
			using = "posteriors")

		expect_true(is.data.frame(results))
	}
)


testthat::test_that(

	desc = "Test complex model",

	code = {
		model <- make_model("Y2 <- X -> Y1; X <-> Y1; X <-> Y2") %>%
			       set_restrictions("Y2[X=1] > Y2[X=0]") %>%
			       set_priors(statement = "Y1[X=1] > Y1[X=0]", alphas = 3)

		data <- simulate_data(model, n = 5)

		posterior <- update_model(model, data, refresh = 0)

    posterior_parameter_draw <- make_parameters(posterior, param_type = "posterior_draw")
    expect_true(length(posterior_parameter_draw) == 16)

		results <- query_model(
			posterior,
			queries = list(COE = "c(Y1[X=1] > Y1[X=0])"),
			given = c("X==1 & Y1==1"),
			using = "posteriors")

		expect_true(is.data.frame(results))
	}
)




testthat::test_that(

	desc = "update_model using keep_fit",

	code = {
		updated <- update_model(make_model("X->Y"), keep_fit = TRUE, refresh = 0)
		expect_true(class(updated) == "causal_model")
	}
)


testthat::test_that(

	desc = "Check long and short data",

	code = {
		model <- make_model('X->Y')
		data_long   <- make_data(model, n = 4)
		data_short  <- collapse_data(data_long, model)
		expect_error(update_model(model, data_short, refresh = 0))
		updated <- update_model(model, data_short, data_type = 'compact', refresh = 0)
		expect_true(class(updated) == "causal_model")
	}
)


testthat::test_that(

	desc = "Test stan arguments",

	code = {
		updated <- update_model(make_model("X->Y"), keep_fit = TRUE, refresh = 0, control = list(adapt_delta = 0.5))
		expect_true(class(updated) == "causal_model")
		updated <- update_model(make_model("X->Y"), keep_fit = TRUE, refresh = 0, control = list(max_treedepth = 20))
		expect_true(class(updated) == "causal_model")
	}
)

testthat::test_that(

	desc = "Test when all NA",

	code = {
		model <- make_model("X -> Y")
		X <- c(NA, NA, NA)
		Y <- c(NA, NA, NA)
		data <- as.data.frame(cbind(X, Y))
		expect_message(update_model(model, data))
		expect_error(update_model(model, data, data_type = "compact"))
	}
)

