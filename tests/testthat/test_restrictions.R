




context(desc = "Testing restrictions")

testthat::skip_on_cran()

dags <- c("X -> Y", "X -> M -> Y", "X -> Y; Z -> Y")

exogenous  <- c("X[] == 0", "X[] == 0", "X[] == 0")
monotonicity  <- c("Y[X = 0] > Y[X=1]", "Y[M = 0] > Y[M =1]",
                   "Y[X = 0, Z = 0] > Y[X = 1, Z = 1]")
cases   <- c("Y==0", "Y[X = 0] > Y[X=1]", "Y == 0")

for(i in length(dags)){

	testthat::test_that(

		desc = "Simple restrictions on exogenous nodes work",

		code = {
			model <- make_model(dags[i]) |>
							 set_restrictions(exogenous[i])

			expect_equal(model$nodal_types$X, "1")
		}
	)

	testthat::test_that(

		desc = "Monotonicity restrictions work",

		code = {
			model <- make_model(dags[i])
		  rest_model <-	set_restrictions(model, monotonicity[i])
			expect_true(
			  length(get_nodal_types(model)$Y) > length(get_nodal_types(rest_model)$Y)
			)
		}
	)

	testthat::test_that(

		desc = "Restriction function errors when it should",

		code = {
			model <- make_model(dags[i])
			expect_error(set_restrictions(model,cases[i]))
			expect_error(make_model("X -> Y -> Z; X <-> Z") |>
			                set_restrictions(list(decreasing('X','Y'), decreasing('Y','Z')), given = c('X.0')))

		}
	)

}

testthat::test_that(

	desc = "error when keep is not logical",

	code = {
		model <- make_model("Y <- X")
		expect_error(model <- set_restrictions(model, "Y[X=1] > Y[X=0]",
		                                       keep="HELLO"))
	}
)


testthat::test_that(

	desc = "errors when labels are wrong",

	code = {
		model <- make_model("Y <- X")
		expect_error(model <- set_restrictions(model, labels = list(X="666")))
		expect_error(model <- set_restrictions(model, labels = list(W=1)))
	}
)



testthat::test_that(

  desc = "sequential restricting types with confounds",

  code = {
    model  <-

      make_model("X->Y; X <-> Y") |>
      set_restrictions(labels = list(Y = c("01")),
                       given = c('X.1'))|>
      set_restrictions(labels = list(Y = c("11")),
                       given = c('X.1'))

    expect_true(nrow(model$parameters_df)==8)
    expect_true(all(get_ambiguities_matrix(model) |>
                      apply(2, sum) == c(2,2,2)))
  }
)

testthat::test_that(

  desc = "restrict by param_names with keep = FALSE",

  code = {
    model  <-

      make_model("X->Y; X <-> Y") |>
      set_restrictions(param_names = c("X.0", "Y.11_X.1"))

    expect_true(nrow(model$parameters_df)==4)
    expect_true(all(get_ambiguities_matrix(model) |> apply(2, sum)== c(2,1)))
  }
)

testthat::test_that(

  desc = "restrict too much",

  code = {
    model  <-

   expect_message(make_model("X->Y; X <-> Y") |>
                           set_restrictions(param_names = c("X.0", "Y.11_X.1"),
                                            keep = TRUE),
                         "parameters_df is empty")
  }
)


testthat::test_that(

  desc = "reduce to single causal type by param_names using  keep = FALSE",

  code = {
    model  <-

      make_model("X->Y; X <-> Y") |>
      set_restrictions(param_names = c("X.0", "Y.11_X.0"), keep = TRUE)

    expect_true(nrow(model$parameters_df)==2)
    expect_true(all(get_ambiguities_matrix(model) |> apply(2, sum)== 1))
  }
)



testthat::test_that(

  desc = "distributions removed if restrictions re-set",

  code = {
    model  <-
      make_model("X->Y") |>
      update_model() |>
      set_prior_distribution() |>
      set_restrictions(statement = "Y[X=1] > Y[X=0]")

    expect_true(is.null(model$posterior_distribution))
    expect_true(is.null(model$prior_distribution))
    expect_true(is.null(model$stan_objects))

  }
)


