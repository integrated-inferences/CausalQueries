# Package index

## Make models

Declare a causal model and set structure, restrictions, parameters, and
priors.

- [`make_model()`](https://integrated-inferences.github.io/CausalQueries/reference/make_model.md)
  : Make a model
- [`simplify_model()`](https://integrated-inferences.github.io/CausalQueries/reference/simplify_model.md)
  [`set_nodal_restrictions()`](https://integrated-inferences.github.io/CausalQueries/reference/simplify_model.md)
  : Simplify nodal types (drop interactions, impose monotonicity)
- [`set_restrictions()`](https://integrated-inferences.github.io/CausalQueries/reference/set_restrictions.md)
  : Restrict a model
- [`set_confound()`](https://integrated-inferences.github.io/CausalQueries/reference/set_confound.md)
  : Set confound
- [`make_parameters()`](https://integrated-inferences.github.io/CausalQueries/reference/parameter_setting.md)
  [`set_parameters()`](https://integrated-inferences.github.io/CausalQueries/reference/parameter_setting.md)
  [`get_parameters()`](https://integrated-inferences.github.io/CausalQueries/reference/parameter_setting.md)
  : Setting parameters
- [`make_priors()`](https://integrated-inferences.github.io/CausalQueries/reference/prior_setting.md)
  [`set_priors()`](https://integrated-inferences.github.io/CausalQueries/reference/prior_setting.md)
  [`get_priors()`](https://integrated-inferences.github.io/CausalQueries/reference/prior_setting.md)
  : Setting priors
- [`set_prior_distribution()`](https://integrated-inferences.github.io/CausalQueries/reference/set_prior_distribution.md)
  : Add prior distribution draws

## Inspect models

Look inside a model — components, summaries, and DAG plots.

- [`inspect()`](https://integrated-inferences.github.io/CausalQueries/reference/inspection.md)
  [`grab()`](https://integrated-inferences.github.io/CausalQueries/reference/inspection.md)
  : Helpers for inspecting causal models
- [`interpret_type()`](https://integrated-inferences.github.io/CausalQueries/reference/interpret_type.md)
  : Interpret or find position in nodal type
- [`get_all_data_types()`](https://integrated-inferences.github.io/CausalQueries/reference/get_all_data_types.md)
  : Get all data types
- [`get_event_probabilities()`](https://integrated-inferences.github.io/CausalQueries/reference/get_event_probabilities.md)
  : Draw complete-data event probabilities
- [`get_query_types()`](https://integrated-inferences.github.io/CausalQueries/reference/get_query_types.md)
  : Look up query types
- [`summary(`*`<causal_model>`*`)`](https://integrated-inferences.github.io/CausalQueries/reference/summary.causal_model.md)
  [`print(`*`<summary.causal_model>`*`)`](https://integrated-inferences.github.io/CausalQueries/reference/summary.causal_model.md)
  : Summarizing causal models
- [`plot_model()`](https://integrated-inferences.github.io/CausalQueries/reference/plot_model.md)
  [`plot(`*`<causal_model>`*`)`](https://integrated-inferences.github.io/CausalQueries/reference/plot_model.md)
  : Plots a DAG in ggplot style using a causal model input

## Update models

Bayesian updating given data (Stan).

- [`update_model()`](https://integrated-inferences.github.io/CausalQueries/reference/update_model.md)
  : Fit causal model using 'stan'

## Query models

Estimands and distributions over queries; plot query results.

- [`query_model()`](https://integrated-inferences.github.io/CausalQueries/reference/query_model.md)
  : Generate data frame for batches of causal queries
- [`query_distribution()`](https://integrated-inferences.github.io/CausalQueries/reference/query_distribution.md)
  : Calculate query distribution
- [`plot(`*`<model_query>`*`)`](https://integrated-inferences.github.io/CausalQueries/reference/plot.model_query.md)
  : Plot model query results
- [`summary(`*`<model_query>`*`)`](https://integrated-inferences.github.io/CausalQueries/reference/summary.model_query.md)
  [`print(`*`<summary.model_query>`*`)`](https://integrated-inferences.github.io/CausalQueries/reference/summary.model_query.md)
  : Summarizing model queries

## Data

Simulate, collapse, and expand data; package example datasets.

- [`collapse_data()`](https://integrated-inferences.github.io/CausalQueries/reference/data_helpers.md)
  [`expand_data()`](https://integrated-inferences.github.io/CausalQueries/reference/data_helpers.md)
  [`make_data()`](https://integrated-inferences.github.io/CausalQueries/reference/data_helpers.md)
  [`make_events()`](https://integrated-inferences.github.io/CausalQueries/reference/data_helpers.md)
  : Data helpers
- [`democracy_data`](https://integrated-inferences.github.io/CausalQueries/reference/democracy_data.md)
  : Development and Democratization: Data for replication of analysis in
  \*Integrated Inferences\*
- [`institutions_data`](https://integrated-inferences.github.io/CausalQueries/reference/institutions_data.md)
  : Institutions and growth: Data for replication of analysis in
  \*Integrated Inferences\*
- [`lipids_data`](https://integrated-inferences.github.io/CausalQueries/reference/lipids_data.md)
  : Lipids: Data for Chickering and Pearl replication

## Query helpers

Compact helpers for common query phrases (effects, monotonicity,
interactions).

- [`increasing()`](https://integrated-inferences.github.io/CausalQueries/reference/query_helpers.md)
  [`non_decreasing()`](https://integrated-inferences.github.io/CausalQueries/reference/query_helpers.md)
  [`decreasing()`](https://integrated-inferences.github.io/CausalQueries/reference/query_helpers.md)
  [`non_increasing()`](https://integrated-inferences.github.io/CausalQueries/reference/query_helpers.md)
  [`interacts()`](https://integrated-inferences.github.io/CausalQueries/reference/query_helpers.md)
  [`complements()`](https://integrated-inferences.github.io/CausalQueries/reference/query_helpers.md)
  [`substitutes()`](https://integrated-inferences.github.io/CausalQueries/reference/query_helpers.md)
  [`te()`](https://integrated-inferences.github.io/CausalQueries/reference/query_helpers.md)
  : Query helpers

## Realise outcomes

Map nodal / causal types to outcomes under interventions.

- [`realise_outcomes()`](https://integrated-inferences.github.io/CausalQueries/reference/realise_outcomes.md)
  : Realise outcomes
- [`draw_causal_type()`](https://integrated-inferences.github.io/CausalQueries/reference/draw_causal_type.md)
  : Draw a single causal type given a parameter vector

## Package

- [`CausalQueries`](https://integrated-inferences.github.io/CausalQueries/reference/CausalQueries-package.md)
  [`CausalQueries-package`](https://integrated-inferences.github.io/CausalQueries/reference/CausalQueries-package.md)
  : CausalQueries: Make, Update, and Query Binary Causal Models
