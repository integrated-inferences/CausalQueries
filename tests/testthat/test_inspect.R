
context("Inspect function check")

testthat::skip_on_cran()
testthat::test_that(

	desc = "Proper summaries.",

	code = {
	#   # PROPOSAL FOR NEW TEST CODE
	#
	#   model <- make_model("X -> Y")
	#   inspect(model, what = "statement")
	#   inspect(model, what = "dag")
	#   inspect(model, what = "nodes")
	#   inspect(model, what = "parents_df")
	#   inspect(model, what = "parameters")
	#   inspect(model, what = "parameter_names")
	#   inspect(model, what = "parameter_mapping")
	#   inspect(model, what = "parameter_matrix")
	#   inspect(model, what = "parameters_df")
	#   inspect(model, what = "causal_types")
	#   inspect(model, what = "nodal_types")
	#   inspect(model, what = "data_types")
	#   inspect(model, what = "ambiguities_matrix")
	#   inspect(model, what = "type_prior")
	#   inspect(model, what = "prior_hyperparameters")
	#   inspect(model, what = "event_probabilities")
	#   inspect(model, what = "posterior_event_probabilities")
	#   inspect(model, what = "prior_distribution")
	#   inspect(model, what = "posterior_distribution")
	#   inspect(model, what = "posterior_event_probabilities")
	#   inspect(model, what = "type_distribution")
	#   inspect(model, what = "data")
	#   inspect(model, what = "stanfit")
	#   inspect(model, what = "stan_summary")
	#
	#   data_long   <- CausalQueries:::simulate_data(model, n = 4)
	#   data_short  <- collapse_data(data_long, model)
	#
	#   model <- update_model(model, keep_type_distribution = FALSE, keep_fit = FALSE)
	#
	#   inspect(model, what = "posterior_distribution")
	#   inspect(model, what = "posterior_event_probabilities")
	#   inspect(model, what = "type_distribution")
	#   inspect(model, what = "data")
	#
	#   x <- inspect(model, what = "stan_summary")
	#   inspect(model, what = "stan_objects")
	#
	#   model <-  update_model(model, data_long)
	#
	#   inspect(model, what = "data")
	#
	#   model <-  update_model(model, data_short)
	#
	#   inspect(model, what = "data")
	#
	#   model <- update_model(model,
	#                         keep_type_distribution = TRUE,
	#                         keep_event_probabilities = TRUE)
	#
	#   inspect(model, what = "posterior_event_probabilities")
	#   inspect(model, what = "type_distribution")
	#
	#   # OLD TESTS
	#   model <-
	#     make_model("X->Y") |>
	#     set_prior_distribution() |>
	#     update_model(data.frame(X=1), keep_event_probabilities = TRUE, keep_fit = TRUE)
	#
	#   for(j in 1:length(args)){
	#     print(paste(j, args[j]))
	#     expect_true((grab(model, args[j]) |> class())[1] == classes[j])
	#   }
	#
	#   # Check options
	#   expect_equal(grab(model, "prior_hyperparameters", "Y") |> length(), 4)
	#
	#   # Proper dimensions
	#   expect_equal(grab(model, "type_prior") |> dim(), c(8, 4000))
	#   expect_equal(grab(model, "type_distribution") |> dim(), c(8, 4000))
	#   expect_equal(grab(model, "posterior_distribution") |> dim(), c(4000, 6))
	#   expect_equal(grab(model, "prior_distribution") |> dim(), c(4000, 6))
	#
	#   # Proper errors
	#   model <- make_model("X->Y")
	#   expect_error(grab(model, "not here"))
	#   expect_error(model |> grab("posterior_distribution"))
	#   expect_error(model |> grab("posterior_event_probabilities"))
	#   expect_error(model |> grab("stan_objects"))
	#   expect_error(model |> grab("stan_fit"))
	#   expect_error(model |> grab("stan_summary"))
	#
	#   expect_true(make_model("X->Y") |>
	#     update_model() |> grab("data") |> is.null())
	#
	#   # Print methods
	#   out <- capture.output(print(grab(model, object = "nodes")))
	#   expect_true(any(grepl("Nodes:", out)))
	#   out <- capture.output(print(grab(model, object = "parents_df")))
	#   expect_true(any(grepl("parents", out)))
	#   out <- capture.output(print(grab(model, object = "parameters_df")))
	#   expect_false(any(grepl("first 10 rows:", out)))
	#   model <- make_model("X -> Y <- M; X -> M")
	#   out <- capture.output(print(grab(model, object = "parameters_df")))
	#   expect_true(any(grepl("first 10 rows:", out)))
	}

)



