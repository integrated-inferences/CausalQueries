
context(desc = "Testing restrictions")


dags <- c("X -> Y", "X -> M -> Y", "X -> Y; Z -> Y")

exogenous  <- c("X[] == 0", "X[] == 0", "X[] == 0")
monotonicity  <- c("Y[X = 0] > Y[X=1]", "Y[M = 0] > Y[M =1]", "Y[X = 0, Z = 0] > Y[X = 1, Z = 1]")
cases   <- c("Y==0", "Y[X = 0] > Y[X=1]", "Y == 0")

for(i in length(dags)){

	testthat::test_that(
		desc = "Simple restrictions on exogenous nodes work",
		code = {
			model <- make_model(dags[i]) %>%
							 set_restrictions(exogenous[i])

			expect_equal(model$nodal_types$X, "1")
		}
	)

	testthat::test_that(
		desc = "Monotonicity restrictions work",
		code = {
			model <- make_model(dags[i])
		  rest_model <-	set_restrictions(model, monotonicity[i])
			expect_true(length(get_nodal_types(model)$Y) > length(get_nodal_types(rest_model)$Y) )
		}
	)

	testthat::test_that(
		desc = "Restriction function errors when it should",
		code = {
			model <- make_model(dags[i])
			expect_error(set_restrictions(model,cases[i]))
		}
	)

}
