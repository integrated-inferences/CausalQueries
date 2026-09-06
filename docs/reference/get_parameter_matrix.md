# Get parameter matrix

Return parameter matrix if it exists; otherwise calculate it assuming no
confounding. The parameter matrix maps from parameters into causal
types. In models without confounding parameters correspond to nodal
types.

## Usage

``` r
get_parameter_matrix(model)
```

## Arguments

- model:

  A model created by
  [`make_model()`](https://integrated-inferences.github.io/CausalQueries/reference/make_model.md)

## Value

A `data.frame`, the parameter matrix, mapping from parameters to causal
types
