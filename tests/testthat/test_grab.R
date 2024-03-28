
context("Grab function check classes")

testthat::skip_on_cran()
testthat::test_that(

	desc = "Proper summaries.",

	code = {

	   args <- c(
	    "causal_statement",
	    "dag",
	    "nodes",
	    "parents",
	    "parameters_df",
	    "causal_types",
	    "causal_types_interpretation",
	    "nodal_types",
	    "data_types",
	    "event_probabilities",
	    "ambiguities_matrix",
	    "parameters",
	    "parameter_names",
	    "parameter_mapping",
	    "parameter_matrix",
	    "prior_hyperparameters",
	    "prior_distribution",
	    "posterior_distribution",
	    "posterior_event_probabilities",
	    "stan_objects",
	    "stan_fit",
	    "stan_summary",
	    "type_prior",
	    "type_posterior"
	  )

	  classes <- c(
	    "statement",
	    "dag",
	    "nodes",
	    "parents",
	    "parameters_df",
	    "causal_types",
	    "list",
	    "nodal_types",
	    "data.frame",
	    "event_probabilities",
	    "matrix",
	    "parameters",
	    "character",
	    "matrix",
	    "parameter_matrix",
	    "numeric",
	    "parameters_prior",
	    "parameters_posterior",
	    "posterior_event_probabilities",
	    "stan_objects",
	    "stanfit",
	    "stan_summary",
	    "type_prior",
	    "type_posterior"
	  )


	  model <- make_model("X->Y") |>
	    set_prior_distribution() |>
	    update_model(keep_event_probabilities = TRUE, keep_fit = TRUE)

	  for(j in 1:length(args)){
	    print(paste(j, args[j]))
	    expect_true((grab(model, args[j]) |> class())[1] == classes[j])
	  }

	  # Check options
	  expect_equal(grab(model, "prior_hyperparameters", "Y") |> length(), 4)

	  # Proper dimensions
	  expect_equal(grab(model, "type_prior") |> dim(), c(4000, 8))
	  expect_equal(grab(model, "type_posterior") |> dim(), c(4000, 8))
	  expect_equal(grab(model, "posterior_distribution") |> dim(), c(4000, 6))
	  expect_equal(grab(model, "prior_distribution") |> dim(), c(4000, 6))

	  # Proper errors
	  model <- make_model("X->Y")
	  expect_error(grab(model, "not here"))
	  expect_error(model |> grab("posterior_distribution"))
	  expect_error(model |> grab("posterior_event_probabilities"))
	  expect_error(model |> grab("stan_objects"))
	  expect_error(model |> grab("stan_fit"))
	  expect_error(model |> grab("stan_summary"))

	}
)


