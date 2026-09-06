# make_par_values

helper to generate filter commands specifying rows of parameters_df that
should be altered given an a vector of parameter names

## Usage

``` r
construct_commands_param_names(param_names, model_param_names)
```

## Arguments

- param_names:

  vector of strings. The name of specific parameter in the form of, for
  example, 'X.1', 'Y.01'

- model_param_names:

  vector of strings. Parameter names found in the model.

## Value

string specifying a filter command
