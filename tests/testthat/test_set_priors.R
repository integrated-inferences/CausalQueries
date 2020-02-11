context("Testing set_priors")

testthat::test_that(
	desc = "Check warnings",
	code = {
		model <- make_model("X -> Y")
		expect_warning(make_priors(model, label = c("X0", "Y1"), alphas = c(1)))
		expect_warning(make_priors(model, statement = c("Y[M=0] > Y[M=1]"), alphas = c(NA)))
		model <- make_model("X -> Y")
		expect_warning(make_priors(model, label = c("X0", "Y1"), distribution = c("jeffreys")))
		expect_warning(make_priors(model, node = c("X"), distribution = c( "uniform"), alphas = c(2)))
	}
)

testthat::test_that(
	desc = "Check errors.",
	code = {
		model <- make_model("X -> M -> Y")
		expect_error(make_priors(model, statement = "Y[X=1] > Y[X=0]", alphas = 3))
		expect_error(make_priors(model, statement = c("Y[M=0] > Y[M=1]"), alphas = c(-1)))
		expect_error(make_priors(model, statement = "X == 1", alphas = c(2, 0.5))) # Trying to replace  6  parameters with  2 values
		expect_error(make_priors(model, distribution = c("certainty", "certainty", "jeffreys", "certainty"), node = c("X", "M", "Y", "Z")))
		expect_error(make_priors(model, node = c("Z"), distribution = c( "uniform"))) # listed node must be in model
	}
)

testthat::test_that(
	desc = "Check output.",
	code = {
		out <- make_model("X -> Y") %>%
  		set_confound(list(X = "Y[X=1]>Y[X=0]"))%>%
  		make_priors(statement = "X==1",
      confound = list(X = "Y[X=1]>Y[X=0]", X = "Y[X=1]<Y[X=0]"),
      alphas = c(2, .5))
		expect_true(all(c(2, .5) %in% out))
	}
)
