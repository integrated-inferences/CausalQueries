# Summarizing model queries

summary method for class "`model_query`".

## Usage

``` r
# S3 method for class 'model_query'
summary(object, ...)

# S3 method for class 'summary.model_query'
print(x, ...)
```

## Arguments

- object:

  An object of `model_query` class produced using `query_model`

- ...:

  Further arguments passed to or from other methods.

- x:

  an object of `model_query` class produced using `query_model`

## Value

Returns the object of class `summary.model_query`

## Examples

``` r
# \donttest{
model <-
  make_model("X -> Y") |>
  query_model("Y[X=1] > Y[X=1]")  |>
  summary()
# }
```
