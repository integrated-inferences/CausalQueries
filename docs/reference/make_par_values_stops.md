# make_par_values_stops

helper to remove stops and reduce complexity of make_par_values

## Usage

``` r
make_par_values_stops(
  model,
  alter = "priors",
  x = NA,
  alter_at = NA,
  node = NA,
  label = NA,
  nodal_type = NA,
  param_set = NA,
  given = NA,
  statement = NA,
  join_by = "|",
  param_names = NA,
  distribution = NA,
  normalize = FALSE
)
```

## Arguments

- model:

  model created with `make_model`

- alter:

  character vector with one of "priors" or "param_value" specifying what
  to alter

- x:

  vector of real non negative values to be substituted into "priors" or
  "param_value"

- alter_at:

  string specifying filtering operations to be applied to parameters_df,
  yielding a logical vector indicating parameters for which values
  should be altered. (see examples)

- node:

  string indicating nodes which are to be altered

- label:

  string. Label for nodal type indicating nodal types for which values
  are to be altered. Equivalent to nodal_type.

- nodal_type:

  string. Label for nodal type indicating nodal types for which values
  are to be altered

- param_set:

  string indicating the name of the set of parameters to be altered

- given:

  string indicates the node on which the parameter to be altered depends

- statement:

  causal query that determines nodal types for which values are to be
  altered

- join_by:

  string specifying the logical operator joining expanded types when
  `statement` contains wildcards. Can take values `'&'` (logical AND) or
  `'|'` (logical OR).

- param_names:

  vector of strings. The name of specific parameter in the form of, for
  example, 'X.1', 'Y.01'

- distribution:

  string indicating a common prior distribution (uniform, jeffreys or
  certainty)

- normalize:

  logical. If TRUE normalizes such that param set probabilities sum to
  1.
