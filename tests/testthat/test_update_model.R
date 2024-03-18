





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
		data <- make_data(XY_noconf, n = 1, parameters = c(.5, .5, .2, .4, .2, .2))
		expect_equal(dim(data), c(1,2))

		updated <-
		  posterior <-
		  suppressWarnings(update_model(XY_noconf, data, refresh = 0))
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
		XY_conf <- make_model("X -> Y")%>%
		  set_confound(., list(X = "Y"))

		expect_equal(dim(get_parameter_matrix(XY_conf)), c(12,8))

		parameters <- c(.5, .5, .5, .5, .5, .5, .5, .5, .1, .7, .1, .1)
		data <- make_data(XY_conf, n = 1, parameters = parameters)
		expect_equal(c(1, 2), dim(data))


		posterior <- suppressWarnings(update_model(XY_conf, data, refresh = 0,
		                                           keep_type_distribution = TRUE))
		expect_true(!is.null(posterior))

		posterior <- suppressWarnings(update_model(XY_conf, data, refresh = 0))
		expect_true(!is.null(posterior))

		prior_ate <- query_distribution(model = posterior,
																			 query = "(Y[X=1] - Y[X=0])",
																			 using = "priors")
		post_ate <- query_distribution(posterior, "c(Y[X=1] - Y[X=0])",
		                               using = "posteriors")

		results <- query_model(
			posterior,
			queries = list(COE = "c(Y[X=1] > Y[X=0])"),
			given = c("X==1 & Y==1"),
			using = "posteriors")

		expect_true(is.data.frame(results))

		expect_error(set_confound(XY_conf))
	}
)


testthat::test_that(

	desc = "Test functions on mediator model (X -> M -> Y)",

	code = {
		XY_mediator <- make_model("X -> M -> Y")

		data <- make_data(
			XY_mediator, n = 1,
			parameters = c(.5, .5, .2, .8, 0, 0, 0, .8, 0, .2))
		expect_equal(c(1, 3), dim(data))

		posterior <- suppressWarnings(update_model(XY_mediator, data, refresh = 0))
		expect_true(!is.null(posterior))

		expect_equal(nrow(make_data(posterior , n = 5, using = "posteriors")), 5)

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

		data <- make_data(
			XY_moderator, n = 1,
			parameters = c(.5, .5, .5, .5,
										 .02, .02, .02, .02, .02, .02, .02, .02,
										 .02, .70, .02, .02, .02, .02, .02, .02))
		expect_equal(c(1, 3), dim(data))

		posterior <- suppressWarnings(update_model(XY_moderator, data, refresh = 0))
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
		model <- make_model("Y2 <- X -> Y1; X <-> Y1; X <-> Y2") |>
			       set_restrictions("Y2[X=1] > Y2[X=0]")
		model <- suppressWarnings(set_priors(model,
		                                     statement = "Y1[X=1] > Y1[X=0]",
		                                     param_set = c("Y1.X.0","Y1.X.1"),
		                                     alphas = c(3,3)))

		data <- make_data(model, n = 5)

		posterior <- suppressWarnings(update_model(model, data, refresh = 0))

    posterior_parameter_draw <- make_parameters(posterior,
                                                param_type = "posterior_draw")
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

	desc = "update_model: keep_event_probabilities and keep_type_distribution",

	code = {
		updated <- suppressWarnings(update_model(make_model("X->Y"),
		                                         keep_event_probabilities = TRUE,
		                                         keep_type_distribution = TRUE,
		                                         refresh = 0,
		                                         keep_fit = TRUE  ))
		expect_true(grepl("X1Y1", updated$stan_objects$stanfit_print[15]))
		expect_true(grepl("X0.Y00", updated$stan_objects$stanfit_print[16]))

		updated <- suppressWarnings(update_model(make_model("X->Y"),
		                                         keep_event_probabilities = FALSE,
		                                         keep_type_distribution = FALSE,
		                                         refresh = 0,
		                                         keep_fit = TRUE  ))
		expect_true(grepl("Y.11", updated$stan_objects$stanfit_print[11]))
		expect_true(grepl("lp__", updated$stan_objects$stanfit_print[12]))

		updated <- suppressWarnings(update_model(make_model("X->Y"),
		                                         keep_event_probabilities = TRUE,
		                                         keep_type_distribution = FALSE,
		                                         refresh = 0,
		                                         keep_fit = TRUE  ))
		expect_true(grepl("Y.11", updated$stan_objects$stanfit_print[11]))
		expect_true(grepl("X0Y0", updated$stan_objects$stanfit_print[12]))

	}
)

testthat::test_that(

	desc = "Check long and short data",

	code = {
		model <- make_model('X->Y')
		data_long   <- make_data(model, n = 4)
		data_short  <- collapse_data(data_long, model)
		updated <- suppressWarnings(update_model(model,
		                                         data_short,
		                                         data_type = 'compact',
		                                         refresh = 0))
		expect_true(class(updated) == "causal_model")
	}
)


testthat::test_that(

	desc = "Test stan arguments and saved stan_objects",

	code = {
	  updated <- suppressWarnings(update_model(
	    make_model("X->Y"),
	    refresh = 0,
	    control = list(adapt_delta = 0.5)
	  ))
	  expect_true(is.null(updated$stan_objects$stan_fit))
		expect_true(class(updated) == "causal_model")
		expect_length(updated$stan_objects, 3)

		updated <- suppressWarnings(update_model(
		  make_model("X->Y"),
		  keep_fit = TRUE,
		  refresh = 0,
		  control = list(max_treedepth = 20)
		))
		expect_true(class(updated$stan_objects$stanfit) == "stanfit")
		expect_true(class(updated) == "causal_model")
		expect_length(updated$stan_objects, 4)
	}
)

testthat::test_that(

	desc = "Test data input",

	code = {
		model <- make_model("X -> Y")
		X <- c(NA, NA, NA)
		Y <- c(NA, NA, NA)
		data <- as.data.frame(cbind(X, Y))
		expect_message(update_model(model, data))
		expect_error(update_model(model, data, data_type = "compact"))

		expect_error(update_model(make_model("X->Y") , data = data.frame(Z = 0)))
	}
)


testthat::test_that(

  desc = "degenerate nodes",

  code = {
    model <- make_model("X->Y") |> set_restrictions("X[]==1")
    updated <- suppressWarnings(update_model(model, iter = 600))
    expect_true(class(updated) == "causal_model")
  }
)



testthat::test_that(

  desc = "priors returned",

  code = {
    set.seed(1)
    model <- make_model("X->Y") |> set_priors(alpha = c(1, 2, 1, 2, 3, 4))
    updated <- suppressWarnings(update_model(model, iter = 2000))
    x <- updated$posterior_distribution |> apply(2, mean)
    expect_true(max(abs(x - c(.33, .66, .1, .2, .3, .4))) < .015)
  }
)


testthat::test_that(

  desc = "posterior returned",

  code = {
    set.seed(1)
    model <- make_model("X->Y")
    updated <- suppressWarnings(
      update_model(model,
                   data = data.frame(X = rep(0:1, 50), Y =rep(0:1, 50)),
                   iter = 2000))
    x <- updated$posterior_distribution |> apply(2, mean)
    expect_true(max(abs(x - c(.5, .5, .02, .01, .95, .02))) < .01)
  }
)



