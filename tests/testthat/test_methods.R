
context("causal_model summary method check")

testthat::skip_on_cran()
testthat::test_that(

	desc = "Proper summaries.",

	code = {
	  # model <-
	  #   make_model('X -> Y')
	  #
	  # print(summary(model), include = "type_distribution")
	  # print(summary(model), include = "posterior_distribution")
	  # print(summary(model), include = "posterior_event_probabilities")
	  #
	  #
	  # model <-
	  #   make_model('X -> Y') |>
	  #   update_model(
	  #     keep_event_probabilities = TRUE,
	  #     keep_fit = TRUE,
	  #     data = CausalQueries:::simulate_data(model, n = 100))
	  #
	  # summary(model)
	  #
	  # print(summary(model), include = "statement")
	  # print(summary(model), include = "dag")
	  # print(summary(model), include = "nodes")
	  # print(summary(model), include = "data_types")
	  # print(summary(model), include = c("statement", "nodes"))
	  # print(summary(model), include = c("dag", "statement", "nodes"))
	  # print(summary(model), include = "parents")
	  # print(summary(model), include = "parents_df")
	  # print(summary(model), include = "parameters")
	  # print(summary(model), include = "parameters_df")
	  # print(summary(model), include = "parameter_names")
	  # print(summary(model), include = "parameter_mapping")
	  # print(summary(model), include = "parameter_matrix")
	  # print(summary(model), include = "causal_types")
	  # print(summary(model), include = "nodal_types")
	  # print(summary(model), include = "data_types")
	  # print(summary(model), include = "ambiguities_matrix")
	  # print(summary(model), include = "type_prior")
	  # print(summary(model), include = "prior_hyperparameters")
	  # print(summary(model), include = "event_probabilities")
	  # print(summary(model), include = "posterior_event_probabilities")
	  # print(summary(model), include = "prior_distribution")
	  # print(summary(model), include = "posterior_distribution")
	  # print(summary(model), include = "type_distribution")
	  # print(summary(model), include = "data")
	  # print(summary(model), include = "stanfit")
	  # print(summary(model), include = "stan_objects")
	  # print(summary(model), include = "stan_summary")
	}

)



