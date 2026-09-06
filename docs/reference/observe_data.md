# Observe data, given a strategy

Observe data, given a strategy

## Usage

``` r
observe_data(
  complete_data,
  observed = NULL,
  nodes_to_observe = NULL,
  prob = 1,
  m = NULL,
  subset = TRUE
)
```

## Arguments

- complete_data:

  A `data.frame`. Data observed and unobserved.

- observed:

  A `data.frame`. Data observed.

- nodes_to_observe:

  A list. Nodes to observe.

- prob:

  A scalar. Observation probability.

- m:

  A integer. Number of units to observe; if specified, `m` overrides
  `prob`.

- subset:

  A character. Logical statement that can be applied to rows of complete
  data. For instance observation for some nodes might depend on observed
  values of other nodes; or observation may only be sought if data not
  already observed!

## Value

A `data.frame` with logical values indicating which nodes to observe in
each row of \`complete_data\`.

## See also

Other data_generation:
[`data_helpers`](https://integrated-inferences.github.io/CausalQueries/reference/data_helpers.md),
[`get_all_data_types()`](https://integrated-inferences.github.io/CausalQueries/reference/get_all_data_types.md),
[`make_data_single()`](https://integrated-inferences.github.io/CausalQueries/reference/make_data_single.md)

## Examples

``` r
model <- make_model("X -> Y")
df <- make_data(model, n = 8)
# Observe X values only
CausalQueries:::observe_data(complete_data = df, nodes_to_observe = "X")
#>      X     Y
#> 1 TRUE FALSE
#> 2 TRUE FALSE
#> 3 TRUE FALSE
#> 4 TRUE FALSE
#> 5 TRUE FALSE
#> 6 TRUE FALSE
#> 7 TRUE FALSE
#> 8 TRUE FALSE
# Observe half the Y values for cases with observed X = 1
CausalQueries:::observe_data(complete_data = df,
     observed = CausalQueries:::observe_data(complete_data = df, nodes_to_observe = "X"),
     nodes_to_observe = "Y", prob = .5,
     subset = "X==1")
#>      X     Y
#> 1 TRUE FALSE
#> 2 TRUE FALSE
#> 3 TRUE FALSE
#> 4 TRUE FALSE
#> 5 TRUE  TRUE
#> 6 TRUE  TRUE
#> 7 TRUE FALSE
#> 8 TRUE FALSE
```
