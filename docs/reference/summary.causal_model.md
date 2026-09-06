# Summarizing causal models

summary method for class "`causal_model`".

## Usage

``` r
# S3 method for class 'causal_model'
summary(object, include = NULL, ...)

# S3 method for class 'summary.causal_model'
print(x, what = NULL, ...)
```

## Arguments

- object:

  An object of `causal_model` class produced using
  [`make_model`](https://integrated-inferences.github.io/CausalQueries/reference/make_model.md)
  or
  [`update_model`](https://integrated-inferences.github.io/CausalQueries/reference/update_model.md).

- include:

  A character string specifying the additional objects to include in
  summary. Defaults to `NULL`. See details for full list of available
  values.

- ...:

  Further arguments passed to or from other methods.

- x:

  An object of `summary.causal_model` class, produced using
  `summary.causal_model`.

- what:

  A character string specifying the objects summaries to print. Defaults
  to `NULL` printing causal statement, specification of nodal types and
  summary of model restrictions. See details for full list of available
  values.

## Value

Returns the object of class `summary.causal_model` that preserves the
list structure of `causal_model` class and adds the following additional
objects:

- `"parents"` a list of parents of all nodes in a model,

- `"parameters"` a vector of 'true' parameters,

- `"parameter_names"` a vector of names of parameters,

- `"data_types"` a list with the all data types consistent with the
  model; for options see
  [`?get_all_data_types`](https://integrated-inferences.github.io/CausalQueries/reference/get_all_data_types.md),

- `"prior_event_probabilities"` a vector of complete-data event
  probabilities `w` given a parameter vector; for options see
  [`?get_event_probabilities`](https://integrated-inferences.github.io/CausalQueries/reference/get_event_probabilities.md),

- `"prior_hyperparameters"` a vector of alpha values used to
  parameterize Dirichlet prior distributions; optionally provide node
  names to reduce output `inspect(prior_hyperparameters, c('M', 'Y'))`

## Details

In addition to the default objects included in \`summary.causal_model\`
users can request additional objects via \`include\` argument. Note that
these additional objects can be large for complex models and can
increase computing time. The \`include\` argument can be a vector of any
of the following additional objects:

- `"parameter_matrix"` A matrix mapping from parameters into causal
  types,

- `"parameter_mapping"` a matrix mapping from parameters into data
  types,

- `"causal_types"` A data frame listing causal types and the nodal types
  that produce them,

- `"prior_distribution"` A data frame of draws from the parameter prior,

- `"ambiguities_matrix"` A matrix mapping from causal types into data
  types,

- `"type_prior"` A matrix of causal-type probabilities under the
  parameter prior.

`print.summary.causal_model` reports causal statement, full
specification of nodal types and summary of model restrictions. By
specifying \`what\` argument users can instead print a custom summary of
any set of the following objects contained in the
\`summary.causal_model\`:

- `"statement"` A character string giving the causal statement,

- `"nodes"` A list containing the nodes in the model,

- `"parents"` A list of parents of all nodes in a model,

- `"parents_df"` A data frame listing nodes, whether they are root nodes
  or not, and the number and names of parents they have,

- `"parameters"` A vector of 'true' parameters,

- `"parameters_df"` A data frame containing parameter information,

- `"parameter_names"` A vector of names of parameters,

- `"parameter_mapping"` A matrix mapping from parameters into data
  types,

- `"parameter_matrix"` A matrix mapping from parameters into causal
  types,

- `"causal_types"` A data frame listing causal types and the nodal types
  that produce them,

- `"nodal_types"` A list with the nodal types of the model,

- `"data_types"` A list with the all data types consistent with the
  model; for options see
  [`?get_all_data_types`](https://integrated-inferences.github.io/CausalQueries/reference/get_all_data_types.md),

- `"prior_hyperparameters"` A vector of alpha values used to
  parameterize Dirichlet prior distributions; optionally provide node
  names to reduce output `inspect(prior_hyperparameters, c('M', 'Y'))`

- `"prior_distribution"` A data frame of draws from the parameter prior,

- `"prior_event_probabilities"` Complete-data event probabilities `w`
  given a single parameter vector; see
  [`?get_event_probabilities`](https://integrated-inferences.github.io/CausalQueries/reference/get_event_probabilities.md),

- `"ambiguities_matrix"` A matrix mapping from causal types into data
  types,

- `"type_prior"` A matrix of causal-type probabilities under the
  parameter prior,

- `"type_posterior"` Posterior draws over causal types (draws \\\times\\
  types). Retained only after legacy updates with
  `keep_type_distribution = TRUE` (flag name vs object name
  `type_posterior`); otherwise see the error from `inspect` / `grab` for
  alternatives,

- `"posterior_distribution"` A data frame of draws from the parameter
  posterior,

- `"posterior_event_probabilities"` Posterior draws of complete-data
  event probabilities `w`,

- `"data"` A data frame with data that was used to update model,

- `"stanfit"` A \`stanfit\` object generated by Stan,

- `"stan_summary"` A \`stanfit\` summary with updated parameter names.

## Examples

``` r
# \donttest{
model <-
  make_model("X -> Y")

model |>
  update_model(
    keep_event_probabilities = TRUE,
    keep_fit = TRUE,
    data = make_data(model, n = 100)
  ) |>
  summary()
#> 
#> SAMPLING FOR MODEL 'simplexes_factorized' NOW (CHAIN 1).
#> Chain 1: 
#> Chain 1: Gradient evaluation took 1.3e-05 seconds
#> Chain 1: 1000 transitions using 10 leapfrog steps per transition would take 0.13 seconds.
#> Chain 1: Adjust your expectations accordingly!
#> Chain 1: 
#> Chain 1: 
#> Chain 1: Iteration:    1 / 2000 [  0%]  (Warmup)
#> Chain 1: Iteration:  200 / 2000 [ 10%]  (Warmup)
#> Chain 1: Iteration:  400 / 2000 [ 20%]  (Warmup)
#> Chain 1: Iteration:  600 / 2000 [ 30%]  (Warmup)
#> Chain 1: Iteration:  800 / 2000 [ 40%]  (Warmup)
#> Chain 1: Iteration: 1000 / 2000 [ 50%]  (Warmup)
#> Chain 1: Iteration: 1001 / 2000 [ 50%]  (Sampling)
#> Chain 1: Iteration: 1200 / 2000 [ 60%]  (Sampling)
#> Chain 1: Iteration: 1400 / 2000 [ 70%]  (Sampling)
#> Chain 1: Iteration: 1600 / 2000 [ 80%]  (Sampling)
#> Chain 1: Iteration: 1800 / 2000 [ 90%]  (Sampling)
#> Chain 1: Iteration: 2000 / 2000 [100%]  (Sampling)
#> Chain 1: 
#> Chain 1:  Elapsed Time: 0.141 seconds (Warm-up)
#> Chain 1:                0.146 seconds (Sampling)
#> Chain 1:                0.287 seconds (Total)
#> Chain 1: 
#> 
#> SAMPLING FOR MODEL 'simplexes_factorized' NOW (CHAIN 2).
#> Chain 2: 
#> Chain 2: Gradient evaluation took 1.3e-05 seconds
#> Chain 2: 1000 transitions using 10 leapfrog steps per transition would take 0.13 seconds.
#> Chain 2: Adjust your expectations accordingly!
#> Chain 2: 
#> Chain 2: 
#> Chain 2: Iteration:    1 / 2000 [  0%]  (Warmup)
#> Chain 2: Iteration:  200 / 2000 [ 10%]  (Warmup)
#> Chain 2: Iteration:  400 / 2000 [ 20%]  (Warmup)
#> Chain 2: Iteration:  600 / 2000 [ 30%]  (Warmup)
#> Chain 2: Iteration:  800 / 2000 [ 40%]  (Warmup)
#> Chain 2: Iteration: 1000 / 2000 [ 50%]  (Warmup)
#> Chain 2: Iteration: 1001 / 2000 [ 50%]  (Sampling)
#> Chain 2: Iteration: 1200 / 2000 [ 60%]  (Sampling)
#> Chain 2: Iteration: 1400 / 2000 [ 70%]  (Sampling)
#> Chain 2: Iteration: 1600 / 2000 [ 80%]  (Sampling)
#> Chain 2: Iteration: 1800 / 2000 [ 90%]  (Sampling)
#> Chain 2: Iteration: 2000 / 2000 [100%]  (Sampling)
#> Chain 2: 
#> Chain 2:  Elapsed Time: 0.127 seconds (Warm-up)
#> Chain 2:                0.149 seconds (Sampling)
#> Chain 2:                0.276 seconds (Total)
#> Chain 2: 
#> 
#> SAMPLING FOR MODEL 'simplexes_factorized' NOW (CHAIN 3).
#> Chain 3: 
#> Chain 3: Gradient evaluation took 1.1e-05 seconds
#> Chain 3: 1000 transitions using 10 leapfrog steps per transition would take 0.11 seconds.
#> Chain 3: Adjust your expectations accordingly!
#> Chain 3: 
#> Chain 3: 
#> Chain 3: Iteration:    1 / 2000 [  0%]  (Warmup)
#> Chain 3: Iteration:  200 / 2000 [ 10%]  (Warmup)
#> Chain 3: Iteration:  400 / 2000 [ 20%]  (Warmup)
#> Chain 3: Iteration:  600 / 2000 [ 30%]  (Warmup)
#> Chain 3: Iteration:  800 / 2000 [ 40%]  (Warmup)
#> Chain 3: Iteration: 1000 / 2000 [ 50%]  (Warmup)
#> Chain 3: Iteration: 1001 / 2000 [ 50%]  (Sampling)
#> Chain 3: Iteration: 1200 / 2000 [ 60%]  (Sampling)
#> Chain 3: Iteration: 1400 / 2000 [ 70%]  (Sampling)
#> Chain 3: Iteration: 1600 / 2000 [ 80%]  (Sampling)
#> Chain 3: Iteration: 1800 / 2000 [ 90%]  (Sampling)
#> Chain 3: Iteration: 2000 / 2000 [100%]  (Sampling)
#> Chain 3: 
#> Chain 3:  Elapsed Time: 0.12 seconds (Warm-up)
#> Chain 3:                0.11 seconds (Sampling)
#> Chain 3:                0.23 seconds (Total)
#> Chain 3: 
#> 
#> SAMPLING FOR MODEL 'simplexes_factorized' NOW (CHAIN 4).
#> Chain 4: 
#> Chain 4: Gradient evaluation took 1e-05 seconds
#> Chain 4: 1000 transitions using 10 leapfrog steps per transition would take 0.1 seconds.
#> Chain 4: Adjust your expectations accordingly!
#> Chain 4: 
#> Chain 4: 
#> Chain 4: Iteration:    1 / 2000 [  0%]  (Warmup)
#> Chain 4: Iteration:  200 / 2000 [ 10%]  (Warmup)
#> Chain 4: Iteration:  400 / 2000 [ 20%]  (Warmup)
#> Chain 4: Iteration:  600 / 2000 [ 30%]  (Warmup)
#> Chain 4: Iteration:  800 / 2000 [ 40%]  (Warmup)
#> Chain 4: Iteration: 1000 / 2000 [ 50%]  (Warmup)
#> Chain 4: Iteration: 1001 / 2000 [ 50%]  (Sampling)
#> Chain 4: Iteration: 1200 / 2000 [ 60%]  (Sampling)
#> Chain 4: Iteration: 1400 / 2000 [ 70%]  (Sampling)
#> Chain 4: Iteration: 1600 / 2000 [ 80%]  (Sampling)
#> Chain 4: Iteration: 1800 / 2000 [ 90%]  (Sampling)
#> Chain 4: Iteration: 2000 / 2000 [100%]  (Sampling)
#> Chain 4: 
#> Chain 4:  Elapsed Time: 0.154 seconds (Warm-up)
#> Chain 4:                0.165 seconds (Sampling)
#> Chain 4:                0.319 seconds (Total)
#> Chain 4: 
#> 
#> Causal statement: 
#> X -> Y
#> 
#> Nodal types: 
#> 
#> Nodal types for X:
#> 0  1
#> 
#> Nodal types for Y:
#> 00  10  01  11
#> 
#> Guide to interpreting nodal types for Y:
#> 
#>   index  interpretation
#> 1    *-  Y = * if X = 0
#> 2    -*  Y = * if X = 1
#> 
#> Number of nodal types by node:
#> X Y 
#> 2 4 
#> 
#> Model has been updated and contains a posterior distribution with
#> 4 chains, each with iter=2000; warmup=1000; thin=1;  
#> Use inspect(model, 'stan_summary') to inspect stan summary
#> 
#> Note: Model does not contain the following objects: type_posterior;
#> to include these objects update model with 'legacy = TRUE, keep_type_distribution = TRUE' (or use query_model / posterior_distribution; see ?inspect)
#> 
#> Note: To pose causal queries of this model use query_model()
#> 
# }

# \donttest{
model <-
  make_model("X -> Y")

model <-
  model |>
  update_model(
    keep_event_probabilities = TRUE,
    keep_fit = TRUE,
    data = make_data(model, n = 100)
  )
#> 
#> SAMPLING FOR MODEL 'simplexes_factorized' NOW (CHAIN 1).
#> Chain 1: 
#> Chain 1: Gradient evaluation took 1.3e-05 seconds
#> Chain 1: 1000 transitions using 10 leapfrog steps per transition would take 0.13 seconds.
#> Chain 1: Adjust your expectations accordingly!
#> Chain 1: 
#> Chain 1: 
#> Chain 1: Iteration:    1 / 2000 [  0%]  (Warmup)
#> Chain 1: Iteration:  200 / 2000 [ 10%]  (Warmup)
#> Chain 1: Iteration:  400 / 2000 [ 20%]  (Warmup)
#> Chain 1: Iteration:  600 / 2000 [ 30%]  (Warmup)
#> Chain 1: Iteration:  800 / 2000 [ 40%]  (Warmup)
#> Chain 1: Iteration: 1000 / 2000 [ 50%]  (Warmup)
#> Chain 1: Iteration: 1001 / 2000 [ 50%]  (Sampling)
#> Chain 1: Iteration: 1200 / 2000 [ 60%]  (Sampling)
#> Chain 1: Iteration: 1400 / 2000 [ 70%]  (Sampling)
#> Chain 1: Iteration: 1600 / 2000 [ 80%]  (Sampling)
#> Chain 1: Iteration: 1800 / 2000 [ 90%]  (Sampling)
#> Chain 1: Iteration: 2000 / 2000 [100%]  (Sampling)
#> Chain 1: 
#> Chain 1:  Elapsed Time: 0.138 seconds (Warm-up)
#> Chain 1:                0.126 seconds (Sampling)
#> Chain 1:                0.264 seconds (Total)
#> Chain 1: 
#> 
#> SAMPLING FOR MODEL 'simplexes_factorized' NOW (CHAIN 2).
#> Chain 2: 
#> Chain 2: Gradient evaluation took 1.2e-05 seconds
#> Chain 2: 1000 transitions using 10 leapfrog steps per transition would take 0.12 seconds.
#> Chain 2: Adjust your expectations accordingly!
#> Chain 2: 
#> Chain 2: 
#> Chain 2: Iteration:    1 / 2000 [  0%]  (Warmup)
#> Chain 2: Iteration:  200 / 2000 [ 10%]  (Warmup)
#> Chain 2: Iteration:  400 / 2000 [ 20%]  (Warmup)
#> Chain 2: Iteration:  600 / 2000 [ 30%]  (Warmup)
#> Chain 2: Iteration:  800 / 2000 [ 40%]  (Warmup)
#> Chain 2: Iteration: 1000 / 2000 [ 50%]  (Warmup)
#> Chain 2: Iteration: 1001 / 2000 [ 50%]  (Sampling)
#> Chain 2: Iteration: 1200 / 2000 [ 60%]  (Sampling)
#> Chain 2: Iteration: 1400 / 2000 [ 70%]  (Sampling)
#> Chain 2: Iteration: 1600 / 2000 [ 80%]  (Sampling)
#> Chain 2: Iteration: 1800 / 2000 [ 90%]  (Sampling)
#> Chain 2: Iteration: 2000 / 2000 [100%]  (Sampling)
#> Chain 2: 
#> Chain 2:  Elapsed Time: 0.139 seconds (Warm-up)
#> Chain 2:                0.121 seconds (Sampling)
#> Chain 2:                0.26 seconds (Total)
#> Chain 2: 
#> 
#> SAMPLING FOR MODEL 'simplexes_factorized' NOW (CHAIN 3).
#> Chain 3: 
#> Chain 3: Gradient evaluation took 1.4e-05 seconds
#> Chain 3: 1000 transitions using 10 leapfrog steps per transition would take 0.14 seconds.
#> Chain 3: Adjust your expectations accordingly!
#> Chain 3: 
#> Chain 3: 
#> Chain 3: Iteration:    1 / 2000 [  0%]  (Warmup)
#> Chain 3: Iteration:  200 / 2000 [ 10%]  (Warmup)
#> Chain 3: Iteration:  400 / 2000 [ 20%]  (Warmup)
#> Chain 3: Iteration:  600 / 2000 [ 30%]  (Warmup)
#> Chain 3: Iteration:  800 / 2000 [ 40%]  (Warmup)
#> Chain 3: Iteration: 1000 / 2000 [ 50%]  (Warmup)
#> Chain 3: Iteration: 1001 / 2000 [ 50%]  (Sampling)
#> Chain 3: Iteration: 1200 / 2000 [ 60%]  (Sampling)
#> Chain 3: Iteration: 1400 / 2000 [ 70%]  (Sampling)
#> Chain 3: Iteration: 1600 / 2000 [ 80%]  (Sampling)
#> Chain 3: Iteration: 1800 / 2000 [ 90%]  (Sampling)
#> Chain 3: Iteration: 2000 / 2000 [100%]  (Sampling)
#> Chain 3: 
#> Chain 3:  Elapsed Time: 0.126 seconds (Warm-up)
#> Chain 3:                0.137 seconds (Sampling)
#> Chain 3:                0.263 seconds (Total)
#> Chain 3: 
#> 
#> SAMPLING FOR MODEL 'simplexes_factorized' NOW (CHAIN 4).
#> Chain 4: 
#> Chain 4: Gradient evaluation took 1.3e-05 seconds
#> Chain 4: 1000 transitions using 10 leapfrog steps per transition would take 0.13 seconds.
#> Chain 4: Adjust your expectations accordingly!
#> Chain 4: 
#> Chain 4: 
#> Chain 4: Iteration:    1 / 2000 [  0%]  (Warmup)
#> Chain 4: Iteration:  200 / 2000 [ 10%]  (Warmup)
#> Chain 4: Iteration:  400 / 2000 [ 20%]  (Warmup)
#> Chain 4: Iteration:  600 / 2000 [ 30%]  (Warmup)
#> Chain 4: Iteration:  800 / 2000 [ 40%]  (Warmup)
#> Chain 4: Iteration: 1000 / 2000 [ 50%]  (Warmup)
#> Chain 4: Iteration: 1001 / 2000 [ 50%]  (Sampling)
#> Chain 4: Iteration: 1200 / 2000 [ 60%]  (Sampling)
#> Chain 4: Iteration: 1400 / 2000 [ 70%]  (Sampling)
#> Chain 4: Iteration: 1600 / 2000 [ 80%]  (Sampling)
#> Chain 4: Iteration: 1800 / 2000 [ 90%]  (Sampling)
#> Chain 4: Iteration: 2000 / 2000 [100%]  (Sampling)
#> Chain 4: 
#> Chain 4:  Elapsed Time: 0.121 seconds (Warm-up)
#> Chain 4:                0.121 seconds (Sampling)
#> Chain 4:                0.242 seconds (Total)
#> Chain 4: 

print(summary(model), what = "type_posterior")
#> Error: No type_posterior on this model. Factorized updates (legacy = FALSE) keep posterior_distribution (parameter draws), not type_posterior (draws over causal types).
#>   - Parameter posterior: grab(model, "posterior_distribution")
#>   - Posterior for a type or potential-outcome query: query_model(model, query = "...", using = "posteriors")
#>   - Type labels / parameter map (on demand): grab(model, "causal_types"), grab(model, "parameter_matrix")
#>   - To retain type_posterior: update_model(..., legacy = TRUE, keep_type_distribution = TRUE)
print(summary(model), what = "posterior_distribution")
#> 
#> posterior_distribution
#> Summary statistics of model parameters posterior distributions:
#> 
#>   Distributions matrix dimensions are 
#>   4000 rows (draws) by 6 cols (parameters)
#> 
#>      mean   sd
#> X.0  0.50 0.05
#> X.1  0.50 0.05
#> Y.00 0.21 0.12
#> Y.10 0.22 0.13
#> Y.01 0.26 0.13
#> Y.11 0.31 0.13
print(summary(model), what = "posterior_event_probabilities")
#> 
#> posterior_event_probabilities
#> Posterior draws of event probabilities (transformed parameters):
#> (complete-data patterns w; not coarsened w_full)
#> 
#>   Distributions matrix dimensions are 
#>   4000 rows (draws) by 4 cols (events)
#> 
#>      mean   sd
#> X0Y0 0.23 0.04
#> X1Y0 0.21 0.04
#> X0Y1 0.27 0.04
#> X1Y1 0.28 0.04
print(summary(model), what = "data_types")
#> 
#> data_types (Data types):
#> Data frame of all possible data (events) given the model:
#> 
#>      event  X  Y
#> X0Y0  X0Y0  0  0
#> X1Y0  X1Y0  1  0
#> X0Y1  X0Y1  0  1
#> X1Y1  X1Y1  1  1
#> Y0      Y0 NA  0
#> Y1      Y1 NA  1
#> X0      X0  0 NA
#> X1      X1  1 NA
#> None  None NA NA
print(summary(model), what = "prior_hyperparameters")
#> 
#> prior_hyperparameters
#> Alpha parameter values used for Dirichlet prior distributions:
#> 
#>  X.0  X.1 Y.00 Y.10 Y.01 Y.11 
#>    1    1    1    1    1    1 
print(summary(model), what = c("statement", "nodes"))
#> 
#> Causal statement: 
#> X -> Y
#> 
#> Nodes: 
#> X, Y
print(summary(model), what = "parameters_df")
#> 
#> parameters_df
#> Mapping of model parameters to nodal types: 
#> 
#>   param_names: name of parameter
#>   node:        name of endogenous node associated
#>                with the parameter
#>   gen:         partial causal ordering of the
#>                parameter's node
#>   param_set:   parameter groupings forming a simplex
#>   given:       if model has confounding gives
#>                conditioning nodal type
#>   param_value: parameter values
#>   priors:      hyperparameters of the prior
#>                Dirichlet distribution 
#> 
#>   param_names node gen param_set nodal_type given param_value priors
#> 1         X.0    X   1         X          0              0.50      1
#> 2         X.1    X   1         X          1              0.50      1
#> 3        Y.00    Y   2         Y         00              0.25      1
#> 4        Y.10    Y   2         Y         10              0.25      1
#> 5        Y.01    Y   2         Y         01              0.25      1
#> 6        Y.11    Y   2         Y         11              0.25      1
print(summary(model), what = "posterior_event_probabilities")
#> 
#> posterior_event_probabilities
#> Posterior draws of event probabilities (transformed parameters):
#> (complete-data patterns w; not coarsened w_full)
#> 
#>   Distributions matrix dimensions are 
#>   4000 rows (draws) by 4 cols (events)
#> 
#>      mean   sd
#> X0Y0 0.23 0.04
#> X1Y0 0.21 0.04
#> X0Y1 0.27 0.04
#> X1Y1 0.28 0.04
print(summary(model), what = "posterior_distribution")
#> 
#> posterior_distribution
#> Summary statistics of model parameters posterior distributions:
#> 
#>   Distributions matrix dimensions are 
#>   4000 rows (draws) by 6 cols (parameters)
#> 
#>      mean   sd
#> X.0  0.50 0.05
#> X.1  0.50 0.05
#> Y.00 0.21 0.12
#> Y.10 0.22 0.13
#> Y.01 0.26 0.13
#> Y.11 0.31 0.13
print(summary(model), what = "data")
#> 
#> Data used to update the model:
#> 
#> data
#>   Data frame dimensions are 
#>   4 rows by 3 cols
#> 
#>   event strategy count
#> 1  X0Y0       XY    23
#> 2  X1Y0       XY    21
#> 3  X0Y1       XY    27
#> 4  X1Y1       XY    29
print(summary(model), what = "stanfit")
#> 
#> stanfit
#> Stan model summary:
#> Inference for Stan model: simplexes_factorized.
#> 4 chains, each with iter=2000; warmup=1000; thin=1; 
#> post-warmup draws per chain=1000, total post-warmup draws=4000.
#> 
#>              mean se_mean   sd   2.5%    25%    50%    75%  97.5% n_eff Rhat
#> lambdas[1]   0.50    0.00 0.05   0.41   0.47   0.50   0.54   0.60  1385 1.00
#> lambdas[2]   0.50    0.00 0.05   0.40   0.46   0.50   0.53   0.59  1385 1.00
#> lambdas[3]   0.21    0.00 0.12   0.01   0.10   0.21   0.31   0.44   663 1.01
#> lambdas[4]   0.22    0.00 0.13   0.01   0.12   0.22   0.32   0.46   777 1.01
#> lambdas[5]   0.26    0.00 0.13   0.02   0.16   0.26   0.36   0.49   726 1.01
#> lambdas[6]   0.31    0.00 0.13   0.06   0.22   0.32   0.41   0.55   815 1.01
#> w[1]         0.23    0.00 0.04   0.16   0.21   0.23   0.26   0.32  2194 1.00
#> w[2]         0.21    0.00 0.04   0.14   0.19   0.21   0.24   0.30  2431 1.00
#> w[3]         0.27    0.00 0.04   0.19   0.24   0.27   0.30   0.35  2174 1.00
#> w[4]         0.28    0.00 0.04   0.20   0.25   0.28   0.31   0.37  2031 1.00
#> lp__       -14.48    0.07 1.75 -18.99 -15.34 -14.06 -13.20 -12.37   684 1.00
#> 
#> Samples were drawn using NUTS(diag_e) at Sun Sep  6 15:31:29 2026.
#> For each parameter, n_eff is a crude measure of effective sample size,
#> and Rhat is the potential scale reduction factor on split chains (at 
#> convergence, Rhat=1).
print(summary(model), what = "type_posterior")
#> Error: No type_posterior on this model. Factorized updates (legacy = FALSE) keep posterior_distribution (parameter draws), not type_posterior (draws over causal types).
#>   - Parameter posterior: grab(model, "posterior_distribution")
#>   - Posterior for a type or potential-outcome query: query_model(model, query = "...", using = "posteriors")
#>   - Type labels / parameter map (on demand): grab(model, "causal_types"), grab(model, "parameter_matrix")
#>   - To retain type_posterior: update_model(..., legacy = TRUE, keep_type_distribution = TRUE)

# Large objects have to be added to the summary before printing
print(summary(model, include = "ambiguities_matrix"),
  what = "ambiguities_matrix")
#> 
#> ambiguities_matrix (Ambiguities matrix)
#> Mapping from causal types into data types:
#>       X0Y0 X1Y0 X0Y1 X1Y1
#> X0Y00    1    0    0    0
#> X1Y00    0    1    0    0
#> X0Y10    0    0    1    0
#> X1Y10    0    1    0    0
#> X0Y01    1    0    0    0
#> X1Y01    0    0    0    1
#> X0Y11    0    0    1    0
#> X1Y11    0    0    0    1
# }
```
