# make_par_values

This is the one step function for make_priors and make_parameters. See
`make_priors` for more help.

## Usage

``` r
make_par_values(
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

## Examples

``` r

# the below methods can be applied to either priors or
# param_values by specifying the desired option in \code{alter}

model <- CausalQueries::make_model("X -> M -> Y; X <-> Y")

#altering values using \code{alter_at}
CausalQueries:::make_par_values(model = model,
                                x = c(0.5,0.25),
                                alter_at = paste(
                                  "node == 'Y' &",
                                  "nodal_type %in% c('00','01') &",
                                  "given == 'X.0'"))
#>  [1] 1.00 1.00 1.00 1.00 1.00 1.00 0.50 1.00 0.25 1.00 1.00 1.00 1.00 1.00

#altering values using \code{param_names}
CausalQueries:::make_par_values(model = model,
                                x = c(0.5,0.25),
                                param_names = c("Y.10_X.0","Y.10_X.1"))
#>  [1] 1.00 1.00 1.00 1.00 1.00 1.00 1.00 0.50 1.00 1.00 1.00 0.25 1.00 1.00

#altering values using \code{statement}
CausalQueries:::make_par_values(model = model,
                                x = c(0.5,0.25),
                                statement = "Y[M=1] > Y[M=0]")
#> Warning: Possible ambiguity: use additional arguments or check behavior in parameters_df.
#>  [1] 1.00 1.00 1.00 1.00 1.00 1.00 1.00 1.00 0.50 1.00 1.00 1.00 0.25 1.00

#altering values using a combination of other arguments
CausalQueries:::make_par_values(model = model,
x = c(0.5,0.25), node = "Y", nodal_type = c("00","01"), given = "X.0")
#>  [1] 1.00 1.00 1.00 1.00 1.00 1.00 0.50 1.00 0.25 1.00 1.00 1.00 1.00 1.00
```
