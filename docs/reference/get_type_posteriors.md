# helper to get type distributions

helper to get type distributions

## Usage

``` r
get_type_posteriors(jobs, model, n_draws, parameters = NULL)
```

## Arguments

- jobs:

  data frame of argument combinations

- model:

  a list of models

- n_draws:

  integer specifying number of draws from prior distribution

- parameters:

  optional list of parameter vectors

## Value

jobs data frame with a nested column of type distributions
