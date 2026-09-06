# function to make a parameters_df from nodal types

function to make a parameters_df from nodal types

## Usage

``` r
make_parameters_df(nodal_types)
```

## Arguments

- nodal_types:

  a list of nodal types

## Examples

``` r

CausalQueries:::make_parameters_df(list(X = "1", Y = c("01", "10")))
#>   param_names node gen param_set nodal_type given param_value priors
#> 1         X.1    X   1         X          1               1.0      1
#> 2        Y.01    Y   2         Y         01               0.5      1
#> 3        Y.10    Y   2         Y         10               0.5      1
```
