# make_par_values

helper to generate filter commands specifying rows of parameters_df that
should be altered given combinations of nodes, nodal_types, param_sets,
givens and statements

## Usage

``` r
construct_commands_other_args(
  node,
  nodal_type,
  param_set,
  given,
  statement,
  model,
  join_by
)
```

## Arguments

- node:

  string indicating nodes which are to be altered

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

- model:

  model created with `make_model`

- join_by:

  string specifying the logical operator joining expanded types when
  `statement` contains wildcards. Can take values `'&'` (logical AND) or
  `'|'` (logical OR).

## Value

string specifying a filter command
