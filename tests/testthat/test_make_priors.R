context(desc = "Testing that making and setting priors work correctly")


XYmodel <- make_model("X -> Y")
prior_distribution <- c("uniform", "jeffreys", "certainty")
alphas <- c(1, 0.5, 10000)

for(i in 1:length(prior_distribution)){
testthat::test_that(
	desc = "make_priors works with prior_distributions argument",
	code = {
		P          <- get_parameter_matrix(XYmodel)
		n_params   <- nrow(P)
		model      <-set_priors(XYmodel, prior_distribution = prior_distribution[i])
		priors     <- model$priors
		testthat::expect_true(all(rep(alphas[i], n_params) == priors))
	})
}


testthat::test_that(
	desc = "make_priors works with alpha of length 1",
	code = {

		model <- set_priors(model = XYmodel, alphas = list(X = c(X0 = 2)))
		priors <- model$priors
		expect_true(priors["X.X0"] == 2)


		model <- set_priors(model = XYmodel,
												alphas = c(`(Y[X=1] == Y[X=0])`  = 3,  `X == 1` = 3  ))
		priors <- model$priors
		expect_true((priors["Y.Y00"] == 3) &(priors["Y.Y11"] == 3)  &(priors["X.X1"] == 3))

	})



testthat::test_that(
	desc = "make_priors works with alpha of length greater than 1",
	code = {
		model <- set_priors(model = XYmodel,
											  alphas = list(X = c(X0 = 2, `X == 1` = 3),  Y = c(`(Y[X=1] > Y[X=0])` = 3, Y10 = 2, `(Y[X=1] == Y[X=0])`  = 3)))
		priors <- model$priors
		expect_true(all(priors == c(2, 3, 3, 2, 3, 3 )))

		model <- set_priors(model = XYmodel, alphas = 2)
		priors <- model$priors
		P          <- get_parameter_matrix(XYmodel)
		n_params   <- nrow(P)
		expect_true(all(priors == rep(2, n_params)))

		model <- set_priors(model = XYmodel,
												alphas = c(`(Y[X=1] == Y[X=0])`  = 3 ))
		priors <- model$priors
		expect_true((priors["Y.Y00"] == 3) &(priors["Y.Y11"] == 3) )
	})


testthat::test_that(
	desc = "make_priors works with confounded models",
	code = {
		model <- XYmodel %>%
		         set_confound(list(X = "(Y[X=1]>Y[X=0])"))

		# Priors get updated with confounds
		priors <- model$priors
		P          <- get_parameter_matrix(model)
		n_params   <- nrow(P)
		expect_true(all(length(priors) ==  		n_params ))
		expect_true(all(!duplicated(names(priors))))


		model <- set_priors(model ,  alphas =  c(`(X == 0)`   = 2))
		priors <- model$priors
		updated_parameters <- c("X-1.X0", "X.X0")
		expect_true(all(priors[names(priors) %in% 		updated_parameters] == c(2, 2)))
		expect_true(all(priors[!names(priors) %in% updated_parameters] == rep(1, n_params - 2) ))



		model <- set_priors(model = model,
												alphas = c(`(X == 0) & (Y[X=1]>Y[X=0])` = 2))
		priors <- model$priors
		updated_parameters <- c("X-1.X0", "Y.Y01")
		expect_true(all(priors[names(priors) %in% 		updated_parameters] == 2))
		expect_true(all(priors[!names(priors) %in% updated_parameters] == rep(1, n_params - 2) ))


		model <- set_priors(model = model,
												alphas = c(`(X == 0) & (Y[X=1]<=Y[X=0])` = 2))
		priors <- model$priors
		updated_parameters <- c ("X.X0")
		expect_true(all(priors[names(priors) %in% 		updated_parameters] == 2))
		expect_true(all(priors[!names(priors) %in% updated_parameters] == rep(1, n_params - 1) ))

	})


testthat::test_that(
	desc = "make_priors erorrs when it should",
	code = {
	expect_error(	make_priors(model = XYmodel,
														alphas = list(X = c(X0 = 2, `X == 1` = 3),  Y = c(Y00 = 1, `(Y[X=1] > Y[X=0])` = 3, Y01 = 2, `(Y[X=1] == Y[X=0])`  = 3))))

	})



