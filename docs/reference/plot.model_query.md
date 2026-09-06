# Plot model query results

Plot method for class `model_query`. Draws point estimates (and credible
intervals when present) from
[`query_model`](https://integrated-inferences.github.io/CausalQueries/reference/query_model.md)
output, faceted by model when more than one model is in the table.

## Usage

``` r
# S3 method for class 'model_query'
plot(x, ...)
```

## Arguments

- x:

  An object of class `model_query`, usually from
  [`query_model`](https://integrated-inferences.github.io/CausalQueries/reference/query_model.md).

- ...:

  Further arguments (currently unused; included for S3 compatibility).

## Value

A `ggplot` object.

## Examples

``` r
# \donttest{
model <- make_model("X -> Y")
q <- query_model(
  model,
  query = "Y[X=1] - Y[X=0]",
  using = "parameters"
)
plot(q)

# }
```
