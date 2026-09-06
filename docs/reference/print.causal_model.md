# Print a short summary for a causal model

print method for class `causal_model`.

## Usage

``` r
# S3 method for class 'causal_model'
print(x, ...)
```

## Arguments

- x:

  An object of `causal_model` class, usually a result of a call to
  [`make_model`](https://integrated-inferences.github.io/CausalQueries/reference/make_model.md)
  or
  [`update_model`](https://integrated-inferences.github.io/CausalQueries/reference/update_model.md).

- ...:

  Further arguments passed to or from other methods.

## Details

The information regarding the causal model includes the statement
describing causal relations using arrow syntax (e.g. `"X -> Y"`), number
of nodal types per parent in a DAG, and number of causal types.
