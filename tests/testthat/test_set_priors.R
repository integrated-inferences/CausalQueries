
context("Testing set_priors")

testthat::skip_on_cran()
testthat::test_that(

	desc = "Check warnings",

	code = {
		model <- make_model("X -> Y")
		expect_error(make_priros(model = model,
		                         alphas = 0.5,
		                         node = "X",
		                         nodal_type = "00",
		                         label = "00"))
		expect_warning(make_priors(model = model,
		                           alphas = 0.5,
		                           node = "Y",
		                           label = "00"))
	}
)

testthat::test_that(

	desc = "Check errors",

	code = {
	  model <- CausalQueries::make_model("X -> Y")
	  expect_error(set_priors(model = model,
	                          alphas = 0.5,
	                          alter_at = "param_names == 'X.0'",
	                          node = "X"))
	  expect_error(set_priors(model = model,
	                          alphas = 0.5,
	                          param_names = "X.0",
	                          node = "X"))
	  expect_error(set_priors(model = model,
	                          alphas = 0.5,
	                          nodal_type = "00",
	                          statement = "Y[X = 1] > Y[X = 0]"))
	  expect_error(set_priors(model = model,
	                          alphas = 0.5,
	                          node = "Y",
	                          statement = "Y[X=1] > Y[X=0]"))
	  expect_error(set_priors(model = model,
	                          alphas = 0.5,
	                          node = "Y",
	                          distribution = "uniform"))
	  expect_error(set_priors(model = model,
	                          alphas = 0.5,
	                          alter_at = "a == 'X.0'"))
	  expect_error(set_priors(model = model,
	                          alphas = 0.5,
	                          param_names = "abc"))
	}
)

testthat::test_that(

	desc = "Check output.",

	code = {
	  model <- CausalQueries::make_model("X -> Y")
	  out <- set_priors(model = model,
	                    alphas = c(0.5,0.25),
	                    alter_at = "node == 'X' & nodal_type %in% c('0','1')")
	  expect_equal(out$parameters_df$priors,
	               c(X.0 = 0.5,
	                 X.1 = 0.25,
	                 Y.00=1,
	                 Y.10 = 1,
	                 Y.01 = 1,
	                 Y.11 = 1))

	  out <- set_priors(model = model,
	                    alphas = c(0.5,0.25),
	                    node = "X",
	                    nodal_type = c("0","1"))
	  expect_equal(out$parameters_df$priors,
	               c(X.0 = 0.5,
	                 X.1 = 0.25,
	                 Y.00=1,
	                 Y.10 = 1,
	                 Y.01 = 1,
	                 Y.11 = 1))

	  out <- set_priors(model = model,
	                    alphas = 0.5,
	                    param_names = "X.0")
	  expect_equal(out$parameters_df$priors,
	               c(X.0 = 0.5,
	                 X.1 = 1,
	                 Y.00=1,
	                 Y.10 = 1,
	                 Y.01 = 1,
	                 Y.11 = 1))

	  model <- CausalQueries::make_model("X -> Y; X <-> Y")
	  out <- suppressWarnings(set_priors(mode = model,
	                                     alphas = c(0.5,0.25),
	                                     node = "Y",
	                                     nodal_type = c("00","01"),
	                                     given = "X.0",
	                                     param_set = "Y.X.0"))
	  expect_equal(out$parameters_df$priors,
	               c(X.0=1,
	                 X.1 = 1,
	                 Y.00_X.0 = .5,
	                 Y.10_X.0 = 1,
	                 Y.01_X.0 = .25,
	                 Y.11_X.0 = 1,
	                 Y.00_X.1 = 1,
	                 Y.10_X.1 = 1,
	                 Y.01_X.1 = 1,
	                 Y.11_X.1 = 1))

	  out <- suppressWarnings(set_priors(model = model,
	                                     alphas = c(0.5,0.25),
	                                     statement = "Y[X=1] > Y[X=0]",
	                                     param_set = c("Y.X.0","Y.X.1")))
	  expect_equal(out$parameters_df$priors,
	               c(X.0=1,
	                 X.1 = 1,
	                 Y.00_X.0 = 1,
	                 Y.10_X.0 = 1,
	                 Y.01_X.0 = 0.5,
	                 Y.11_X.0 = 1,
	                 Y.00_X.1 =  1,
	                 Y.10_X.1 = 1,
	                 Y.01_X.1 = 0.25,
	                 Y.11_X.1 = 1))
	}
)

