# Make a model

`make_model` uses causal statements encoded as strings to specify the
nodes and edges of a graph. Implied nodal types are calculated and
default priors are provided under the assumption of no confounding.
Models can be updated with restrictions on types and/or informative
priors on parameters.

## Usage

``` r
make_model(
  statement = "X -> Y",
  add_causal_types = TRUE,
  nodal_types = NULL,
  allow_large = FALSE,
  legacy = NULL,
  drop_interactions = NULL,
  keep_interactions = NULL,
  monotone = NULL
)
```

## Arguments

- statement:

  character string. Statement describing causal relations between nodes.
  Directed relations can be specified using '-\>' or '\<-' and can be
  combined. For instance "X -\> Y", "Y \<- X" or "X1 -\> Y \<- X2; X1
  -\> X2". Confounded relations can be specified using a double headed
  arrow, "X \<-\> Y", to indicate unobserved confounding between X and
  Y.

- add_causal_types:

  Logical. Whether to create and attach causal types to `model`. Under
  `legacy = TRUE` defaults to \`TRUE\`. Under `legacy = FALSE` (default)
  causal types are not attached regardless of this argument.

- nodal_types:

  Named list of character vectors of nodal types for \*\*every\*\* node
  (same names and causal order as `model$nodes`). Use this to keep a
  many-parent node small. If `NULL`, types are auto-generated (refused
  for five or more parents on one node).

- allow_large:

  Logical. The number of causal types is the product of the numbers of
  nodal types across nodes. When that product exceeds one million and
  causal types will be built (`add_causal_types = TRUE`), `make_model`
  errors unless `allow_large` is \`TRUE\`, in which case it warns
  instead. Defaults to \`FALSE\`. Setting `add_causal_types = FALSE` or
  `legacy = FALSE` skips attaching that product. Restricted
  `nodal_types` are assessed by their actual lengths, so a many-parent
  node with a small type set is allowed when the product stays below the
  limit.

- legacy:

  Logical. `FALSE` (default) is the new parameters-only / factorized
  path (no global causal-type table at build). `TRUE` restores the
  current causal-type expansion. Override with
  `options(CausalQueries.legacy = TRUE)`.

- drop_interactions:

  Drop interaction orders at least this high when auto-building nodal
  types. See
  [`simplify_model`](https://integrated-inferences.github.io/CausalQueries/reference/simplify_model.md).

- keep_interactions:

  Interaction parent-sets to keep despite `drop_interactions`. See
  [`simplify_model`](https://integrated-inferences.github.io/CausalQueries/reference/simplify_model.md).

- monotone:

  Monotonicity restrictions applied when building nodal types. See
  [`simplify_model`](https://integrated-inferences.github.io/CausalQueries/reference/simplify_model.md).

## Value

An object of class `causal_model`.

An object of class `"causal_model"` is a list containing at least the
following components:

- statement:

  A character vector of the statement that defines the model

- dag:

  A `data.frame` with columns \`parent\`and \`children\` indicating how
  nodes relate to each other.

- nodes:

  A named `list` with the nodes in the model

- parents_df:

  A `data.frame` listing nodes, whether they are root nodes or not, and
  the number of parents they have

- nodal_types:

  Optional: A named `list` with the nodal types in the model. List
  should be ordered according to the causal ordering of nodes. If NULL
  nodal types are generated. If FALSE, a parameters data frame is not
  generated.

- parameters_df:

  A `data.frame` with descriptive information of the parameters in the
  model

- causal_types:

  A `data.frame` listing causal types and the nodal types that produce
  them (legacy / when attached)

By default a causal model has flat (uniform) priors and parameters that
put equal weight on each parameter within each parameter set. The
parameter ranges (range of the nodal types) can be adjusted with
[`set_restrictions`](https://integrated-inferences.github.io/CausalQueries/reference/set_restrictions.md).
The priors can be adjusted with
[`set_priors`](https://integrated-inferences.github.io/CausalQueries/reference/prior_setting.md).
Specific parameter values can be adjusted with
[`set_parameters`](https://integrated-inferences.github.io/CausalQueries/reference/parameter_setting.md).

## Large and many-parent models

The saturated type space grows very fast: a node with \\k\\ binary
parents has \\2^2^k\\ nodal types (2, 4, 16, 256, 65536 for \\k =
1,...,4\\). Practical options:

- Use the default `legacy = FALSE` path so a global causal-type table is
  not attached at build.

- Pass `drop_interactions` and/or `monotone` to build a reduced type set
  at construction (see
  [`simplify_model`](https://integrated-inferences.github.io/CausalQueries/reference/simplify_model.md)),
  e.g. `drop_interactions = TRUE, monotone = "+"`.

- Pass a short `nodal_types` list for busy nodes (required for five or
  more parents).

- On an existing model, call
  [`simplify_model`](https://integrated-inferences.github.io/CausalQueries/reference/simplify_model.md).

- Set `allow_large = TRUE` only if you intentionally build a causal-type
  product above one million under `legacy = TRUE`.

Nodal-type strings are one digit per parent assignment. Assignment order
matches `get_parents()` / the column order in the node's type matrix.

## References

Tietz T, Medina L, Syunyaev G, Humphreys M (2026). "Making, Updating,
and Querying Causal Models with CausalQueries." *Journal of Statistical
Software*, **117**(1), 1–40.
[doi:10.18637/jss.v117.i01](https://doi.org/10.18637/jss.v117.i01) .

## See also

[`summary.causal_model`](https://integrated-inferences.github.io/CausalQueries/reference/summary.causal_model.md)
provides summary method for output objects of class `causal_model`

## Examples

``` r
make_model(statement = "X -> Y")
#> 
#> Causal statement: 
#> X -> Y
#> 
#> Number of nodal types by node:
#> X Y 
#> 2 4 
modelXKY <- make_model("X -> K -> Y; X -> Y")

# Example where a cyclical dag is attempted
if (FALSE) { # \dontrun{
 modelXKX <- make_model("X -> K -> X")
} # }

# Examples with confounding
model <- make_model("X->Y; X <-> Y")
inspect(model, "parameter_matrix")
#> 
#> parameter_matrix:
#> 
#>   rows:   parameters
#>   cols:   causal types
#>   cells:  whether a parameter probability is used
#>           in the calculation of causal type probability
#> 
#>          X0.Y00 X1.Y00 X0.Y10 X1.Y10 X0.Y01 X1.Y01 X0.Y11 X1.Y11
#> X.0           1      0      1      0      1      0      1      0
#> X.1           0      1      0      1      0      1      0      1
#> Y.00_X.0      1      0      0      0      0      0      0      0
#> Y.10_X.0      0      0      1      0      0      0      0      0
#> Y.01_X.0      0      0      0      0      1      0      0      0
#> Y.11_X.0      0      0      0      0      0      0      1      0
#> Y.00_X.1      0      1      0      0      0      0      0      0
#> Y.10_X.1      0      0      0      1      0      0      0      0
#> Y.01_X.1      0      0      0      0      0      1      0      0
#> Y.11_X.1      0      0      0      0      0      0      0      1
model <- make_model("Y2 <- X -> Y1; X <-> Y1; X <-> Y2")
dim(inspect(model, "parameter_matrix"))
#> 
#> parameter_matrix:
#> 
#>   rows:   parameters
#>   cols:   causal types
#>   cells:  whether a parameter probability is used
#>           in the calculation of causal type probability
#> 
#>           X0.Y100.Y200 X1.Y100.Y200 X0.Y110.Y200 X1.Y110.Y200 X0.Y101.Y200
#> X.0                  1            0            1            0            1
#> X.1                  0            1            0            1            0
#> Y1.00_X.0            1            0            0            0            0
#> Y1.10_X.0            0            0            1            0            0
#> Y1.01_X.0            0            0            0            0            1
#> Y1.11_X.0            0            0            0            0            0
#> Y1.00_X.1            0            1            0            0            0
#> Y1.10_X.1            0            0            0            1            0
#> Y1.01_X.1            0            0            0            0            0
#> Y1.11_X.1            0            0            0            0            0
#> Y2.00_X.0            1            0            1            0            1
#> Y2.10_X.0            0            0            0            0            0
#> Y2.01_X.0            0            0            0            0            0
#> Y2.11_X.0            0            0            0            0            0
#> Y2.00_X.1            0            1            0            1            0
#> Y2.10_X.1            0            0            0            0            0
#> Y2.01_X.1            0            0            0            0            0
#> Y2.11_X.1            0            0            0            0            0
#>           X1.Y101.Y200 X0.Y111.Y200 X1.Y111.Y200 X0.Y100.Y210 X1.Y100.Y210
#> X.0                  0            1            0            1            0
#> X.1                  1            0            1            0            1
#> Y1.00_X.0            0            0            0            1            0
#> Y1.10_X.0            0            0            0            0            0
#> Y1.01_X.0            0            0            0            0            0
#> Y1.11_X.0            0            1            0            0            0
#> Y1.00_X.1            0            0            0            0            1
#> Y1.10_X.1            0            0            0            0            0
#> Y1.01_X.1            1            0            0            0            0
#> Y1.11_X.1            0            0            1            0            0
#> Y2.00_X.0            0            1            0            0            0
#> Y2.10_X.0            0            0            0            1            0
#> Y2.01_X.0            0            0            0            0            0
#> Y2.11_X.0            0            0            0            0            0
#> Y2.00_X.1            1            0            1            0            0
#> Y2.10_X.1            0            0            0            0            1
#> Y2.01_X.1            0            0            0            0            0
#> Y2.11_X.1            0            0            0            0            0
#>           X0.Y110.Y210 X1.Y110.Y210 X0.Y101.Y210 X1.Y101.Y210 X0.Y111.Y210
#> X.0                  1            0            1            0            1
#> X.1                  0            1            0            1            0
#> Y1.00_X.0            0            0            0            0            0
#> Y1.10_X.0            1            0            0            0            0
#> Y1.01_X.0            0            0            1            0            0
#> Y1.11_X.0            0            0            0            0            1
#> Y1.00_X.1            0            0            0            0            0
#> Y1.10_X.1            0            1            0            0            0
#> Y1.01_X.1            0            0            0            1            0
#> Y1.11_X.1            0            0            0            0            0
#> Y2.00_X.0            0            0            0            0            0
#> Y2.10_X.0            1            0            1            0            1
#> Y2.01_X.0            0            0            0            0            0
#> Y2.11_X.0            0            0            0            0            0
#> Y2.00_X.1            0            0            0            0            0
#> Y2.10_X.1            0            1            0            1            0
#> Y2.01_X.1            0            0            0            0            0
#> Y2.11_X.1            0            0            0            0            0
#>           X1.Y111.Y210 X0.Y100.Y201 X1.Y100.Y201 X0.Y110.Y201 X1.Y110.Y201
#> X.0                  0            1            0            1            0
#> X.1                  1            0            1            0            1
#> Y1.00_X.0            0            1            0            0            0
#> Y1.10_X.0            0            0            0            1            0
#> Y1.01_X.0            0            0            0            0            0
#> Y1.11_X.0            0            0            0            0            0
#> Y1.00_X.1            0            0            1            0            0
#> Y1.10_X.1            0            0            0            0            1
#> Y1.01_X.1            0            0            0            0            0
#> Y1.11_X.1            1            0            0            0            0
#> Y2.00_X.0            0            0            0            0            0
#> Y2.10_X.0            0            0            0            0            0
#> Y2.01_X.0            0            1            0            1            0
#> Y2.11_X.0            0            0            0            0            0
#> Y2.00_X.1            0            0            0            0            0
#> Y2.10_X.1            1            0            0            0            0
#> Y2.01_X.1            0            0            1            0            1
#> Y2.11_X.1            0            0            0            0            0
#>           X0.Y101.Y201 X1.Y101.Y201 X0.Y111.Y201 X1.Y111.Y201 X0.Y100.Y211
#> X.0                  1            0            1            0            1
#> X.1                  0            1            0            1            0
#> Y1.00_X.0            0            0            0            0            1
#> Y1.10_X.0            0            0            0            0            0
#> Y1.01_X.0            1            0            0            0            0
#> Y1.11_X.0            0            0            1            0            0
#> Y1.00_X.1            0            0            0            0            0
#> Y1.10_X.1            0            0            0            0            0
#> Y1.01_X.1            0            1            0            0            0
#> Y1.11_X.1            0            0            0            1            0
#> Y2.00_X.0            0            0            0            0            0
#> Y2.10_X.0            0            0            0            0            0
#> Y2.01_X.0            1            0            1            0            0
#> Y2.11_X.0            0            0            0            0            1
#> Y2.00_X.1            0            0            0            0            0
#> Y2.10_X.1            0            0            0            0            0
#> Y2.01_X.1            0            1            0            1            0
#> Y2.11_X.1            0            0            0            0            0
#>           X1.Y100.Y211 X0.Y110.Y211 X1.Y110.Y211 X0.Y101.Y211 X1.Y101.Y211
#> X.0                  0            1            0            1            0
#> X.1                  1            0            1            0            1
#> Y1.00_X.0            0            0            0            0            0
#> Y1.10_X.0            0            1            0            0            0
#> Y1.01_X.0            0            0            0            1            0
#> Y1.11_X.0            0            0            0            0            0
#> Y1.00_X.1            1            0            0            0            0
#> Y1.10_X.1            0            0            1            0            0
#> Y1.01_X.1            0            0            0            0            1
#> Y1.11_X.1            0            0            0            0            0
#> Y2.00_X.0            0            0            0            0            0
#> Y2.10_X.0            0            0            0            0            0
#> Y2.01_X.0            0            0            0            0            0
#> Y2.11_X.0            0            1            0            1            0
#> Y2.00_X.1            0            0            0            0            0
#> Y2.10_X.1            0            0            0            0            0
#> Y2.01_X.1            0            0            0            0            0
#> Y2.11_X.1            1            0            1            0            1
#>           X0.Y111.Y211 X1.Y111.Y211
#> X.0                  1            0
#> X.1                  0            1
#> Y1.00_X.0            0            0
#> Y1.10_X.0            0            0
#> Y1.01_X.0            0            0
#> Y1.11_X.0            1            0
#> Y1.00_X.1            0            0
#> Y1.10_X.1            0            0
#> Y1.01_X.1            0            0
#> Y1.11_X.1            0            1
#> Y2.00_X.0            0            0
#> Y2.10_X.0            0            0
#> Y2.01_X.0            0            0
#> Y2.11_X.0            1            0
#> Y2.00_X.1            0            0
#> Y2.10_X.1            0            0
#> Y2.01_X.1            0            0
#> Y2.11_X.1            0            1
#> [1] 18 32
inspect(model, "parameter_matrix")
#> 
#> parameter_matrix:
#> 
#>   rows:   parameters
#>   cols:   causal types
#>   cells:  whether a parameter probability is used
#>           in the calculation of causal type probability
#> 
#>           X0.Y100.Y200 X1.Y100.Y200 X0.Y110.Y200 X1.Y110.Y200 X0.Y101.Y200
#> X.0                  1            0            1            0            1
#> X.1                  0            1            0            1            0
#> Y1.00_X.0            1            0            0            0            0
#> Y1.10_X.0            0            0            1            0            0
#> Y1.01_X.0            0            0            0            0            1
#> Y1.11_X.0            0            0            0            0            0
#> Y1.00_X.1            0            1            0            0            0
#> Y1.10_X.1            0            0            0            1            0
#> Y1.01_X.1            0            0            0            0            0
#> Y1.11_X.1            0            0            0            0            0
#> Y2.00_X.0            1            0            1            0            1
#> Y2.10_X.0            0            0            0            0            0
#> Y2.01_X.0            0            0            0            0            0
#> Y2.11_X.0            0            0            0            0            0
#> Y2.00_X.1            0            1            0            1            0
#> Y2.10_X.1            0            0            0            0            0
#> Y2.01_X.1            0            0            0            0            0
#> Y2.11_X.1            0            0            0            0            0
#>           X1.Y101.Y200 X0.Y111.Y200 X1.Y111.Y200 X0.Y100.Y210 X1.Y100.Y210
#> X.0                  0            1            0            1            0
#> X.1                  1            0            1            0            1
#> Y1.00_X.0            0            0            0            1            0
#> Y1.10_X.0            0            0            0            0            0
#> Y1.01_X.0            0            0            0            0            0
#> Y1.11_X.0            0            1            0            0            0
#> Y1.00_X.1            0            0            0            0            1
#> Y1.10_X.1            0            0            0            0            0
#> Y1.01_X.1            1            0            0            0            0
#> Y1.11_X.1            0            0            1            0            0
#> Y2.00_X.0            0            1            0            0            0
#> Y2.10_X.0            0            0            0            1            0
#> Y2.01_X.0            0            0            0            0            0
#> Y2.11_X.0            0            0            0            0            0
#> Y2.00_X.1            1            0            1            0            0
#> Y2.10_X.1            0            0            0            0            1
#> Y2.01_X.1            0            0            0            0            0
#> Y2.11_X.1            0            0            0            0            0
#>           X0.Y110.Y210 X1.Y110.Y210 X0.Y101.Y210 X1.Y101.Y210 X0.Y111.Y210
#> X.0                  1            0            1            0            1
#> X.1                  0            1            0            1            0
#> Y1.00_X.0            0            0            0            0            0
#> Y1.10_X.0            1            0            0            0            0
#> Y1.01_X.0            0            0            1            0            0
#> Y1.11_X.0            0            0            0            0            1
#> Y1.00_X.1            0            0            0            0            0
#> Y1.10_X.1            0            1            0            0            0
#> Y1.01_X.1            0            0            0            1            0
#> Y1.11_X.1            0            0            0            0            0
#> Y2.00_X.0            0            0            0            0            0
#> Y2.10_X.0            1            0            1            0            1
#> Y2.01_X.0            0            0            0            0            0
#> Y2.11_X.0            0            0            0            0            0
#> Y2.00_X.1            0            0            0            0            0
#> Y2.10_X.1            0            1            0            1            0
#> Y2.01_X.1            0            0            0            0            0
#> Y2.11_X.1            0            0            0            0            0
#>           X1.Y111.Y210 X0.Y100.Y201 X1.Y100.Y201 X0.Y110.Y201 X1.Y110.Y201
#> X.0                  0            1            0            1            0
#> X.1                  1            0            1            0            1
#> Y1.00_X.0            0            1            0            0            0
#> Y1.10_X.0            0            0            0            1            0
#> Y1.01_X.0            0            0            0            0            0
#> Y1.11_X.0            0            0            0            0            0
#> Y1.00_X.1            0            0            1            0            0
#> Y1.10_X.1            0            0            0            0            1
#> Y1.01_X.1            0            0            0            0            0
#> Y1.11_X.1            1            0            0            0            0
#> Y2.00_X.0            0            0            0            0            0
#> Y2.10_X.0            0            0            0            0            0
#> Y2.01_X.0            0            1            0            1            0
#> Y2.11_X.0            0            0            0            0            0
#> Y2.00_X.1            0            0            0            0            0
#> Y2.10_X.1            1            0            0            0            0
#> Y2.01_X.1            0            0            1            0            1
#> Y2.11_X.1            0            0            0            0            0
#>           X0.Y101.Y201 X1.Y101.Y201 X0.Y111.Y201 X1.Y111.Y201 X0.Y100.Y211
#> X.0                  1            0            1            0            1
#> X.1                  0            1            0            1            0
#> Y1.00_X.0            0            0            0            0            1
#> Y1.10_X.0            0            0            0            0            0
#> Y1.01_X.0            1            0            0            0            0
#> Y1.11_X.0            0            0            1            0            0
#> Y1.00_X.1            0            0            0            0            0
#> Y1.10_X.1            0            0            0            0            0
#> Y1.01_X.1            0            1            0            0            0
#> Y1.11_X.1            0            0            0            1            0
#> Y2.00_X.0            0            0            0            0            0
#> Y2.10_X.0            0            0            0            0            0
#> Y2.01_X.0            1            0            1            0            0
#> Y2.11_X.0            0            0            0            0            1
#> Y2.00_X.1            0            0            0            0            0
#> Y2.10_X.1            0            0            0            0            0
#> Y2.01_X.1            0            1            0            1            0
#> Y2.11_X.1            0            0            0            0            0
#>           X1.Y100.Y211 X0.Y110.Y211 X1.Y110.Y211 X0.Y101.Y211 X1.Y101.Y211
#> X.0                  0            1            0            1            0
#> X.1                  1            0            1            0            1
#> Y1.00_X.0            0            0            0            0            0
#> Y1.10_X.0            0            1            0            0            0
#> Y1.01_X.0            0            0            0            1            0
#> Y1.11_X.0            0            0            0            0            0
#> Y1.00_X.1            1            0            0            0            0
#> Y1.10_X.1            0            0            1            0            0
#> Y1.01_X.1            0            0            0            0            1
#> Y1.11_X.1            0            0            0            0            0
#> Y2.00_X.0            0            0            0            0            0
#> Y2.10_X.0            0            0            0            0            0
#> Y2.01_X.0            0            0            0            0            0
#> Y2.11_X.0            0            1            0            1            0
#> Y2.00_X.1            0            0            0            0            0
#> Y2.10_X.1            0            0            0            0            0
#> Y2.01_X.1            0            0            0            0            0
#> Y2.11_X.1            1            0            1            0            1
#>           X0.Y111.Y211 X1.Y111.Y211
#> X.0                  1            0
#> X.1                  0            1
#> Y1.00_X.0            0            0
#> Y1.10_X.0            0            0
#> Y1.01_X.0            0            0
#> Y1.11_X.0            1            0
#> Y1.00_X.1            0            0
#> Y1.10_X.1            0            0
#> Y1.01_X.1            0            0
#> Y1.11_X.1            0            1
#> Y2.00_X.0            0            0
#> Y2.10_X.0            0            0
#> Y2.01_X.0            0            0
#> Y2.11_X.0            1            0
#> Y2.00_X.1            0            0
#> Y2.10_X.1            0            0
#> Y2.01_X.1            0            0
#> Y2.11_X.1            0            1
model <- make_model("X1 -> Y <- X2; X1 <-> Y; X2 <-> Y")
dim(inspect(model, "parameter_matrix"))
#> 
#> parameter_matrix:
#> 
#>   rows:   parameters
#>   cols:   causal types
#>   cells:  whether a parameter probability is used
#>           in the calculation of causal type probability
#> 
#>                  X10.X20.Y0000 X11.X20.Y0000 X10.X21.Y0000 X11.X21.Y0000
#> X1.0                         1             0             1             0
#> X1.1                         0             1             0             1
#> X2.0                         1             1             0             0
#> X2.1                         0             0             1             1
#> Y.0000_X1.0_X2.0             1             0             0             0
#> Y.1000_X1.0_X2.0             0             0             0             0
#> Y.0100_X1.0_X2.0             0             0             0             0
#> Y.1100_X1.0_X2.0             0             0             0             0
#> Y.0010_X1.0_X2.0             0             0             0             0
#> Y.1010_X1.0_X2.0             0             0             0             0
#> Y.0110_X1.0_X2.0             0             0             0             0
#> Y.1110_X1.0_X2.0             0             0             0             0
#> Y.0001_X1.0_X2.0             0             0             0             0
#> Y.1001_X1.0_X2.0             0             0             0             0
#> Y.0101_X1.0_X2.0             0             0             0             0
#> Y.1101_X1.0_X2.0             0             0             0             0
#> Y.0011_X1.0_X2.0             0             0             0             0
#> Y.1011_X1.0_X2.0             0             0             0             0
#> Y.0111_X1.0_X2.0             0             0             0             0
#> Y.1111_X1.0_X2.0             0             0             0             0
#> Y.0000_X1.0_X2.1             0             0             1             0
#> Y.1000_X1.0_X2.1             0             0             0             0
#> Y.0100_X1.0_X2.1             0             0             0             0
#> Y.1100_X1.0_X2.1             0             0             0             0
#> Y.0010_X1.0_X2.1             0             0             0             0
#> Y.1010_X1.0_X2.1             0             0             0             0
#> Y.0110_X1.0_X2.1             0             0             0             0
#> Y.1110_X1.0_X2.1             0             0             0             0
#> Y.0001_X1.0_X2.1             0             0             0             0
#> Y.1001_X1.0_X2.1             0             0             0             0
#> Y.0101_X1.0_X2.1             0             0             0             0
#> Y.1101_X1.0_X2.1             0             0             0             0
#> Y.0011_X1.0_X2.1             0             0             0             0
#> Y.1011_X1.0_X2.1             0             0             0             0
#> Y.0111_X1.0_X2.1             0             0             0             0
#> Y.1111_X1.0_X2.1             0             0             0             0
#> Y.0000_X1.1_X2.0             0             1             0             0
#> Y.1000_X1.1_X2.0             0             0             0             0
#> Y.0100_X1.1_X2.0             0             0             0             0
#> Y.1100_X1.1_X2.0             0             0             0             0
#> Y.0010_X1.1_X2.0             0             0             0             0
#> Y.1010_X1.1_X2.0             0             0             0             0
#> Y.0110_X1.1_X2.0             0             0             0             0
#> Y.1110_X1.1_X2.0             0             0             0             0
#> Y.0001_X1.1_X2.0             0             0             0             0
#> Y.1001_X1.1_X2.0             0             0             0             0
#> Y.0101_X1.1_X2.0             0             0             0             0
#> Y.1101_X1.1_X2.0             0             0             0             0
#> Y.0011_X1.1_X2.0             0             0             0             0
#> Y.1011_X1.1_X2.0             0             0             0             0
#> Y.0111_X1.1_X2.0             0             0             0             0
#> Y.1111_X1.1_X2.0             0             0             0             0
#> Y.0000_X1.1_X2.1             0             0             0             1
#> Y.1000_X1.1_X2.1             0             0             0             0
#> Y.0100_X1.1_X2.1             0             0             0             0
#> Y.1100_X1.1_X2.1             0             0             0             0
#> Y.0010_X1.1_X2.1             0             0             0             0
#> Y.1010_X1.1_X2.1             0             0             0             0
#> Y.0110_X1.1_X2.1             0             0             0             0
#> Y.1110_X1.1_X2.1             0             0             0             0
#> Y.0001_X1.1_X2.1             0             0             0             0
#> Y.1001_X1.1_X2.1             0             0             0             0
#> Y.0101_X1.1_X2.1             0             0             0             0
#> Y.1101_X1.1_X2.1             0             0             0             0
#> Y.0011_X1.1_X2.1             0             0             0             0
#> Y.1011_X1.1_X2.1             0             0             0             0
#> Y.0111_X1.1_X2.1             0             0             0             0
#> Y.1111_X1.1_X2.1             0             0             0             0
#>                  X10.X20.Y1000 X11.X20.Y1000 X10.X21.Y1000 X11.X21.Y1000
#> X1.0                         1             0             1             0
#> X1.1                         0             1             0             1
#> X2.0                         1             1             0             0
#> X2.1                         0             0             1             1
#> Y.0000_X1.0_X2.0             0             0             0             0
#> Y.1000_X1.0_X2.0             1             0             0             0
#> Y.0100_X1.0_X2.0             0             0             0             0
#> Y.1100_X1.0_X2.0             0             0             0             0
#> Y.0010_X1.0_X2.0             0             0             0             0
#> Y.1010_X1.0_X2.0             0             0             0             0
#> Y.0110_X1.0_X2.0             0             0             0             0
#> Y.1110_X1.0_X2.0             0             0             0             0
#> Y.0001_X1.0_X2.0             0             0             0             0
#> Y.1001_X1.0_X2.0             0             0             0             0
#> Y.0101_X1.0_X2.0             0             0             0             0
#> Y.1101_X1.0_X2.0             0             0             0             0
#> Y.0011_X1.0_X2.0             0             0             0             0
#> Y.1011_X1.0_X2.0             0             0             0             0
#> Y.0111_X1.0_X2.0             0             0             0             0
#> Y.1111_X1.0_X2.0             0             0             0             0
#> Y.0000_X1.0_X2.1             0             0             0             0
#> Y.1000_X1.0_X2.1             0             0             1             0
#> Y.0100_X1.0_X2.1             0             0             0             0
#> Y.1100_X1.0_X2.1             0             0             0             0
#> Y.0010_X1.0_X2.1             0             0             0             0
#> Y.1010_X1.0_X2.1             0             0             0             0
#> Y.0110_X1.0_X2.1             0             0             0             0
#> Y.1110_X1.0_X2.1             0             0             0             0
#> Y.0001_X1.0_X2.1             0             0             0             0
#> Y.1001_X1.0_X2.1             0             0             0             0
#> Y.0101_X1.0_X2.1             0             0             0             0
#> Y.1101_X1.0_X2.1             0             0             0             0
#> Y.0011_X1.0_X2.1             0             0             0             0
#> Y.1011_X1.0_X2.1             0             0             0             0
#> Y.0111_X1.0_X2.1             0             0             0             0
#> Y.1111_X1.0_X2.1             0             0             0             0
#> Y.0000_X1.1_X2.0             0             0             0             0
#> Y.1000_X1.1_X2.0             0             1             0             0
#> Y.0100_X1.1_X2.0             0             0             0             0
#> Y.1100_X1.1_X2.0             0             0             0             0
#> Y.0010_X1.1_X2.0             0             0             0             0
#> Y.1010_X1.1_X2.0             0             0             0             0
#> Y.0110_X1.1_X2.0             0             0             0             0
#> Y.1110_X1.1_X2.0             0             0             0             0
#> Y.0001_X1.1_X2.0             0             0             0             0
#> Y.1001_X1.1_X2.0             0             0             0             0
#> Y.0101_X1.1_X2.0             0             0             0             0
#> Y.1101_X1.1_X2.0             0             0             0             0
#> Y.0011_X1.1_X2.0             0             0             0             0
#> Y.1011_X1.1_X2.0             0             0             0             0
#> Y.0111_X1.1_X2.0             0             0             0             0
#> Y.1111_X1.1_X2.0             0             0             0             0
#> Y.0000_X1.1_X2.1             0             0             0             0
#> Y.1000_X1.1_X2.1             0             0             0             1
#> Y.0100_X1.1_X2.1             0             0             0             0
#> Y.1100_X1.1_X2.1             0             0             0             0
#> Y.0010_X1.1_X2.1             0             0             0             0
#> Y.1010_X1.1_X2.1             0             0             0             0
#> Y.0110_X1.1_X2.1             0             0             0             0
#> Y.1110_X1.1_X2.1             0             0             0             0
#> Y.0001_X1.1_X2.1             0             0             0             0
#> Y.1001_X1.1_X2.1             0             0             0             0
#> Y.0101_X1.1_X2.1             0             0             0             0
#> Y.1101_X1.1_X2.1             0             0             0             0
#> Y.0011_X1.1_X2.1             0             0             0             0
#> Y.1011_X1.1_X2.1             0             0             0             0
#> Y.0111_X1.1_X2.1             0             0             0             0
#> Y.1111_X1.1_X2.1             0             0             0             0
#>                  X10.X20.Y0100 X11.X20.Y0100 X10.X21.Y0100 X11.X21.Y0100
#> X1.0                         1             0             1             0
#> X1.1                         0             1             0             1
#> X2.0                         1             1             0             0
#> X2.1                         0             0             1             1
#> Y.0000_X1.0_X2.0             0             0             0             0
#> Y.1000_X1.0_X2.0             0             0             0             0
#> Y.0100_X1.0_X2.0             1             0             0             0
#> Y.1100_X1.0_X2.0             0             0             0             0
#> Y.0010_X1.0_X2.0             0             0             0             0
#> Y.1010_X1.0_X2.0             0             0             0             0
#> Y.0110_X1.0_X2.0             0             0             0             0
#> Y.1110_X1.0_X2.0             0             0             0             0
#> Y.0001_X1.0_X2.0             0             0             0             0
#> Y.1001_X1.0_X2.0             0             0             0             0
#> Y.0101_X1.0_X2.0             0             0             0             0
#> Y.1101_X1.0_X2.0             0             0             0             0
#> Y.0011_X1.0_X2.0             0             0             0             0
#> Y.1011_X1.0_X2.0             0             0             0             0
#> Y.0111_X1.0_X2.0             0             0             0             0
#> Y.1111_X1.0_X2.0             0             0             0             0
#> Y.0000_X1.0_X2.1             0             0             0             0
#> Y.1000_X1.0_X2.1             0             0             0             0
#> Y.0100_X1.0_X2.1             0             0             1             0
#> Y.1100_X1.0_X2.1             0             0             0             0
#> Y.0010_X1.0_X2.1             0             0             0             0
#> Y.1010_X1.0_X2.1             0             0             0             0
#> Y.0110_X1.0_X2.1             0             0             0             0
#> Y.1110_X1.0_X2.1             0             0             0             0
#> Y.0001_X1.0_X2.1             0             0             0             0
#> Y.1001_X1.0_X2.1             0             0             0             0
#> Y.0101_X1.0_X2.1             0             0             0             0
#> Y.1101_X1.0_X2.1             0             0             0             0
#> Y.0011_X1.0_X2.1             0             0             0             0
#> Y.1011_X1.0_X2.1             0             0             0             0
#> Y.0111_X1.0_X2.1             0             0             0             0
#> Y.1111_X1.0_X2.1             0             0             0             0
#> Y.0000_X1.1_X2.0             0             0             0             0
#> Y.1000_X1.1_X2.0             0             0             0             0
#> Y.0100_X1.1_X2.0             0             1             0             0
#> Y.1100_X1.1_X2.0             0             0             0             0
#> Y.0010_X1.1_X2.0             0             0             0             0
#> Y.1010_X1.1_X2.0             0             0             0             0
#> Y.0110_X1.1_X2.0             0             0             0             0
#> Y.1110_X1.1_X2.0             0             0             0             0
#> Y.0001_X1.1_X2.0             0             0             0             0
#> Y.1001_X1.1_X2.0             0             0             0             0
#> Y.0101_X1.1_X2.0             0             0             0             0
#> Y.1101_X1.1_X2.0             0             0             0             0
#> Y.0011_X1.1_X2.0             0             0             0             0
#> Y.1011_X1.1_X2.0             0             0             0             0
#> Y.0111_X1.1_X2.0             0             0             0             0
#> Y.1111_X1.1_X2.0             0             0             0             0
#> Y.0000_X1.1_X2.1             0             0             0             0
#> Y.1000_X1.1_X2.1             0             0             0             0
#> Y.0100_X1.1_X2.1             0             0             0             1
#> Y.1100_X1.1_X2.1             0             0             0             0
#> Y.0010_X1.1_X2.1             0             0             0             0
#> Y.1010_X1.1_X2.1             0             0             0             0
#> Y.0110_X1.1_X2.1             0             0             0             0
#> Y.1110_X1.1_X2.1             0             0             0             0
#> Y.0001_X1.1_X2.1             0             0             0             0
#> Y.1001_X1.1_X2.1             0             0             0             0
#> Y.0101_X1.1_X2.1             0             0             0             0
#> Y.1101_X1.1_X2.1             0             0             0             0
#> Y.0011_X1.1_X2.1             0             0             0             0
#> Y.1011_X1.1_X2.1             0             0             0             0
#> Y.0111_X1.1_X2.1             0             0             0             0
#> Y.1111_X1.1_X2.1             0             0             0             0
#>                  X10.X20.Y1100 X11.X20.Y1100 X10.X21.Y1100 X11.X21.Y1100
#> X1.0                         1             0             1             0
#> X1.1                         0             1             0             1
#> X2.0                         1             1             0             0
#> X2.1                         0             0             1             1
#> Y.0000_X1.0_X2.0             0             0             0             0
#> Y.1000_X1.0_X2.0             0             0             0             0
#> Y.0100_X1.0_X2.0             0             0             0             0
#> Y.1100_X1.0_X2.0             1             0             0             0
#> Y.0010_X1.0_X2.0             0             0             0             0
#> Y.1010_X1.0_X2.0             0             0             0             0
#> Y.0110_X1.0_X2.0             0             0             0             0
#> Y.1110_X1.0_X2.0             0             0             0             0
#> Y.0001_X1.0_X2.0             0             0             0             0
#> Y.1001_X1.0_X2.0             0             0             0             0
#> Y.0101_X1.0_X2.0             0             0             0             0
#> Y.1101_X1.0_X2.0             0             0             0             0
#> Y.0011_X1.0_X2.0             0             0             0             0
#> Y.1011_X1.0_X2.0             0             0             0             0
#> Y.0111_X1.0_X2.0             0             0             0             0
#> Y.1111_X1.0_X2.0             0             0             0             0
#> Y.0000_X1.0_X2.1             0             0             0             0
#> Y.1000_X1.0_X2.1             0             0             0             0
#> Y.0100_X1.0_X2.1             0             0             0             0
#> Y.1100_X1.0_X2.1             0             0             1             0
#> Y.0010_X1.0_X2.1             0             0             0             0
#> Y.1010_X1.0_X2.1             0             0             0             0
#> Y.0110_X1.0_X2.1             0             0             0             0
#> Y.1110_X1.0_X2.1             0             0             0             0
#> Y.0001_X1.0_X2.1             0             0             0             0
#> Y.1001_X1.0_X2.1             0             0             0             0
#> Y.0101_X1.0_X2.1             0             0             0             0
#> Y.1101_X1.0_X2.1             0             0             0             0
#> Y.0011_X1.0_X2.1             0             0             0             0
#> Y.1011_X1.0_X2.1             0             0             0             0
#> Y.0111_X1.0_X2.1             0             0             0             0
#> Y.1111_X1.0_X2.1             0             0             0             0
#> Y.0000_X1.1_X2.0             0             0             0             0
#> Y.1000_X1.1_X2.0             0             0             0             0
#> Y.0100_X1.1_X2.0             0             0             0             0
#> Y.1100_X1.1_X2.0             0             1             0             0
#> Y.0010_X1.1_X2.0             0             0             0             0
#> Y.1010_X1.1_X2.0             0             0             0             0
#> Y.0110_X1.1_X2.0             0             0             0             0
#> Y.1110_X1.1_X2.0             0             0             0             0
#> Y.0001_X1.1_X2.0             0             0             0             0
#> Y.1001_X1.1_X2.0             0             0             0             0
#> Y.0101_X1.1_X2.0             0             0             0             0
#> Y.1101_X1.1_X2.0             0             0             0             0
#> Y.0011_X1.1_X2.0             0             0             0             0
#> Y.1011_X1.1_X2.0             0             0             0             0
#> Y.0111_X1.1_X2.0             0             0             0             0
#> Y.1111_X1.1_X2.0             0             0             0             0
#> Y.0000_X1.1_X2.1             0             0             0             0
#> Y.1000_X1.1_X2.1             0             0             0             0
#> Y.0100_X1.1_X2.1             0             0             0             0
#> Y.1100_X1.1_X2.1             0             0             0             1
#> Y.0010_X1.1_X2.1             0             0             0             0
#> Y.1010_X1.1_X2.1             0             0             0             0
#> Y.0110_X1.1_X2.1             0             0             0             0
#> Y.1110_X1.1_X2.1             0             0             0             0
#> Y.0001_X1.1_X2.1             0             0             0             0
#> Y.1001_X1.1_X2.1             0             0             0             0
#> Y.0101_X1.1_X2.1             0             0             0             0
#> Y.1101_X1.1_X2.1             0             0             0             0
#> Y.0011_X1.1_X2.1             0             0             0             0
#> Y.1011_X1.1_X2.1             0             0             0             0
#> Y.0111_X1.1_X2.1             0             0             0             0
#> Y.1111_X1.1_X2.1             0             0             0             0
#>                  X10.X20.Y0010 X11.X20.Y0010 X10.X21.Y0010 X11.X21.Y0010
#> X1.0                         1             0             1             0
#> X1.1                         0             1             0             1
#> X2.0                         1             1             0             0
#> X2.1                         0             0             1             1
#> Y.0000_X1.0_X2.0             0             0             0             0
#> Y.1000_X1.0_X2.0             0             0             0             0
#> Y.0100_X1.0_X2.0             0             0             0             0
#> Y.1100_X1.0_X2.0             0             0             0             0
#> Y.0010_X1.0_X2.0             1             0             0             0
#> Y.1010_X1.0_X2.0             0             0             0             0
#> Y.0110_X1.0_X2.0             0             0             0             0
#> Y.1110_X1.0_X2.0             0             0             0             0
#> Y.0001_X1.0_X2.0             0             0             0             0
#> Y.1001_X1.0_X2.0             0             0             0             0
#> Y.0101_X1.0_X2.0             0             0             0             0
#> Y.1101_X1.0_X2.0             0             0             0             0
#> Y.0011_X1.0_X2.0             0             0             0             0
#> Y.1011_X1.0_X2.0             0             0             0             0
#> Y.0111_X1.0_X2.0             0             0             0             0
#> Y.1111_X1.0_X2.0             0             0             0             0
#> Y.0000_X1.0_X2.1             0             0             0             0
#> Y.1000_X1.0_X2.1             0             0             0             0
#> Y.0100_X1.0_X2.1             0             0             0             0
#> Y.1100_X1.0_X2.1             0             0             0             0
#> Y.0010_X1.0_X2.1             0             0             1             0
#> Y.1010_X1.0_X2.1             0             0             0             0
#> Y.0110_X1.0_X2.1             0             0             0             0
#> Y.1110_X1.0_X2.1             0             0             0             0
#> Y.0001_X1.0_X2.1             0             0             0             0
#> Y.1001_X1.0_X2.1             0             0             0             0
#> Y.0101_X1.0_X2.1             0             0             0             0
#> Y.1101_X1.0_X2.1             0             0             0             0
#> Y.0011_X1.0_X2.1             0             0             0             0
#> Y.1011_X1.0_X2.1             0             0             0             0
#> Y.0111_X1.0_X2.1             0             0             0             0
#> Y.1111_X1.0_X2.1             0             0             0             0
#> Y.0000_X1.1_X2.0             0             0             0             0
#> Y.1000_X1.1_X2.0             0             0             0             0
#> Y.0100_X1.1_X2.0             0             0             0             0
#> Y.1100_X1.1_X2.0             0             0             0             0
#> Y.0010_X1.1_X2.0             0             1             0             0
#> Y.1010_X1.1_X2.0             0             0             0             0
#> Y.0110_X1.1_X2.0             0             0             0             0
#> Y.1110_X1.1_X2.0             0             0             0             0
#> Y.0001_X1.1_X2.0             0             0             0             0
#> Y.1001_X1.1_X2.0             0             0             0             0
#> Y.0101_X1.1_X2.0             0             0             0             0
#> Y.1101_X1.1_X2.0             0             0             0             0
#> Y.0011_X1.1_X2.0             0             0             0             0
#> Y.1011_X1.1_X2.0             0             0             0             0
#> Y.0111_X1.1_X2.0             0             0             0             0
#> Y.1111_X1.1_X2.0             0             0             0             0
#> Y.0000_X1.1_X2.1             0             0             0             0
#> Y.1000_X1.1_X2.1             0             0             0             0
#> Y.0100_X1.1_X2.1             0             0             0             0
#> Y.1100_X1.1_X2.1             0             0             0             0
#> Y.0010_X1.1_X2.1             0             0             0             1
#> Y.1010_X1.1_X2.1             0             0             0             0
#> Y.0110_X1.1_X2.1             0             0             0             0
#> Y.1110_X1.1_X2.1             0             0             0             0
#> Y.0001_X1.1_X2.1             0             0             0             0
#> Y.1001_X1.1_X2.1             0             0             0             0
#> Y.0101_X1.1_X2.1             0             0             0             0
#> Y.1101_X1.1_X2.1             0             0             0             0
#> Y.0011_X1.1_X2.1             0             0             0             0
#> Y.1011_X1.1_X2.1             0             0             0             0
#> Y.0111_X1.1_X2.1             0             0             0             0
#> Y.1111_X1.1_X2.1             0             0             0             0
#>                  X10.X20.Y1010 X11.X20.Y1010 X10.X21.Y1010 X11.X21.Y1010
#> X1.0                         1             0             1             0
#> X1.1                         0             1             0             1
#> X2.0                         1             1             0             0
#> X2.1                         0             0             1             1
#> Y.0000_X1.0_X2.0             0             0             0             0
#> Y.1000_X1.0_X2.0             0             0             0             0
#> Y.0100_X1.0_X2.0             0             0             0             0
#> Y.1100_X1.0_X2.0             0             0             0             0
#> Y.0010_X1.0_X2.0             0             0             0             0
#> Y.1010_X1.0_X2.0             1             0             0             0
#> Y.0110_X1.0_X2.0             0             0             0             0
#> Y.1110_X1.0_X2.0             0             0             0             0
#> Y.0001_X1.0_X2.0             0             0             0             0
#> Y.1001_X1.0_X2.0             0             0             0             0
#> Y.0101_X1.0_X2.0             0             0             0             0
#> Y.1101_X1.0_X2.0             0             0             0             0
#> Y.0011_X1.0_X2.0             0             0             0             0
#> Y.1011_X1.0_X2.0             0             0             0             0
#> Y.0111_X1.0_X2.0             0             0             0             0
#> Y.1111_X1.0_X2.0             0             0             0             0
#> Y.0000_X1.0_X2.1             0             0             0             0
#> Y.1000_X1.0_X2.1             0             0             0             0
#> Y.0100_X1.0_X2.1             0             0             0             0
#> Y.1100_X1.0_X2.1             0             0             0             0
#> Y.0010_X1.0_X2.1             0             0             0             0
#> Y.1010_X1.0_X2.1             0             0             1             0
#> Y.0110_X1.0_X2.1             0             0             0             0
#> Y.1110_X1.0_X2.1             0             0             0             0
#> Y.0001_X1.0_X2.1             0             0             0             0
#> Y.1001_X1.0_X2.1             0             0             0             0
#> Y.0101_X1.0_X2.1             0             0             0             0
#> Y.1101_X1.0_X2.1             0             0             0             0
#> Y.0011_X1.0_X2.1             0             0             0             0
#> Y.1011_X1.0_X2.1             0             0             0             0
#> Y.0111_X1.0_X2.1             0             0             0             0
#> Y.1111_X1.0_X2.1             0             0             0             0
#> Y.0000_X1.1_X2.0             0             0             0             0
#> Y.1000_X1.1_X2.0             0             0             0             0
#> Y.0100_X1.1_X2.0             0             0             0             0
#> Y.1100_X1.1_X2.0             0             0             0             0
#> Y.0010_X1.1_X2.0             0             0             0             0
#> Y.1010_X1.1_X2.0             0             1             0             0
#> Y.0110_X1.1_X2.0             0             0             0             0
#> Y.1110_X1.1_X2.0             0             0             0             0
#> Y.0001_X1.1_X2.0             0             0             0             0
#> Y.1001_X1.1_X2.0             0             0             0             0
#> Y.0101_X1.1_X2.0             0             0             0             0
#> Y.1101_X1.1_X2.0             0             0             0             0
#> Y.0011_X1.1_X2.0             0             0             0             0
#> Y.1011_X1.1_X2.0             0             0             0             0
#> Y.0111_X1.1_X2.0             0             0             0             0
#> Y.1111_X1.1_X2.0             0             0             0             0
#> Y.0000_X1.1_X2.1             0             0             0             0
#> Y.1000_X1.1_X2.1             0             0             0             0
#> Y.0100_X1.1_X2.1             0             0             0             0
#> Y.1100_X1.1_X2.1             0             0             0             0
#> Y.0010_X1.1_X2.1             0             0             0             0
#> Y.1010_X1.1_X2.1             0             0             0             1
#> Y.0110_X1.1_X2.1             0             0             0             0
#> Y.1110_X1.1_X2.1             0             0             0             0
#> Y.0001_X1.1_X2.1             0             0             0             0
#> Y.1001_X1.1_X2.1             0             0             0             0
#> Y.0101_X1.1_X2.1             0             0             0             0
#> Y.1101_X1.1_X2.1             0             0             0             0
#> Y.0011_X1.1_X2.1             0             0             0             0
#> Y.1011_X1.1_X2.1             0             0             0             0
#> Y.0111_X1.1_X2.1             0             0             0             0
#> Y.1111_X1.1_X2.1             0             0             0             0
#>                  X10.X20.Y0110 X11.X20.Y0110 X10.X21.Y0110 X11.X21.Y0110
#> X1.0                         1             0             1             0
#> X1.1                         0             1             0             1
#> X2.0                         1             1             0             0
#> X2.1                         0             0             1             1
#> Y.0000_X1.0_X2.0             0             0             0             0
#> Y.1000_X1.0_X2.0             0             0             0             0
#> Y.0100_X1.0_X2.0             0             0             0             0
#> Y.1100_X1.0_X2.0             0             0             0             0
#> Y.0010_X1.0_X2.0             0             0             0             0
#> Y.1010_X1.0_X2.0             0             0             0             0
#> Y.0110_X1.0_X2.0             1             0             0             0
#> Y.1110_X1.0_X2.0             0             0             0             0
#> Y.0001_X1.0_X2.0             0             0             0             0
#> Y.1001_X1.0_X2.0             0             0             0             0
#> Y.0101_X1.0_X2.0             0             0             0             0
#> Y.1101_X1.0_X2.0             0             0             0             0
#> Y.0011_X1.0_X2.0             0             0             0             0
#> Y.1011_X1.0_X2.0             0             0             0             0
#> Y.0111_X1.0_X2.0             0             0             0             0
#> Y.1111_X1.0_X2.0             0             0             0             0
#> Y.0000_X1.0_X2.1             0             0             0             0
#> Y.1000_X1.0_X2.1             0             0             0             0
#> Y.0100_X1.0_X2.1             0             0             0             0
#> Y.1100_X1.0_X2.1             0             0             0             0
#> Y.0010_X1.0_X2.1             0             0             0             0
#> Y.1010_X1.0_X2.1             0             0             0             0
#> Y.0110_X1.0_X2.1             0             0             1             0
#> Y.1110_X1.0_X2.1             0             0             0             0
#> Y.0001_X1.0_X2.1             0             0             0             0
#> Y.1001_X1.0_X2.1             0             0             0             0
#> Y.0101_X1.0_X2.1             0             0             0             0
#> Y.1101_X1.0_X2.1             0             0             0             0
#> Y.0011_X1.0_X2.1             0             0             0             0
#> Y.1011_X1.0_X2.1             0             0             0             0
#> Y.0111_X1.0_X2.1             0             0             0             0
#> Y.1111_X1.0_X2.1             0             0             0             0
#> Y.0000_X1.1_X2.0             0             0             0             0
#> Y.1000_X1.1_X2.0             0             0             0             0
#> Y.0100_X1.1_X2.0             0             0             0             0
#> Y.1100_X1.1_X2.0             0             0             0             0
#> Y.0010_X1.1_X2.0             0             0             0             0
#> Y.1010_X1.1_X2.0             0             0             0             0
#> Y.0110_X1.1_X2.0             0             1             0             0
#> Y.1110_X1.1_X2.0             0             0             0             0
#> Y.0001_X1.1_X2.0             0             0             0             0
#> Y.1001_X1.1_X2.0             0             0             0             0
#> Y.0101_X1.1_X2.0             0             0             0             0
#> Y.1101_X1.1_X2.0             0             0             0             0
#> Y.0011_X1.1_X2.0             0             0             0             0
#> Y.1011_X1.1_X2.0             0             0             0             0
#> Y.0111_X1.1_X2.0             0             0             0             0
#> Y.1111_X1.1_X2.0             0             0             0             0
#> Y.0000_X1.1_X2.1             0             0             0             0
#> Y.1000_X1.1_X2.1             0             0             0             0
#> Y.0100_X1.1_X2.1             0             0             0             0
#> Y.1100_X1.1_X2.1             0             0             0             0
#> Y.0010_X1.1_X2.1             0             0             0             0
#> Y.1010_X1.1_X2.1             0             0             0             0
#> Y.0110_X1.1_X2.1             0             0             0             1
#> Y.1110_X1.1_X2.1             0             0             0             0
#> Y.0001_X1.1_X2.1             0             0             0             0
#> Y.1001_X1.1_X2.1             0             0             0             0
#> Y.0101_X1.1_X2.1             0             0             0             0
#> Y.1101_X1.1_X2.1             0             0             0             0
#> Y.0011_X1.1_X2.1             0             0             0             0
#> Y.1011_X1.1_X2.1             0             0             0             0
#> Y.0111_X1.1_X2.1             0             0             0             0
#> Y.1111_X1.1_X2.1             0             0             0             0
#>                  X10.X20.Y1110 X11.X20.Y1110 X10.X21.Y1110 X11.X21.Y1110
#> X1.0                         1             0             1             0
#> X1.1                         0             1             0             1
#> X2.0                         1             1             0             0
#> X2.1                         0             0             1             1
#> Y.0000_X1.0_X2.0             0             0             0             0
#> Y.1000_X1.0_X2.0             0             0             0             0
#> Y.0100_X1.0_X2.0             0             0             0             0
#> Y.1100_X1.0_X2.0             0             0             0             0
#> Y.0010_X1.0_X2.0             0             0             0             0
#> Y.1010_X1.0_X2.0             0             0             0             0
#> Y.0110_X1.0_X2.0             0             0             0             0
#> Y.1110_X1.0_X2.0             1             0             0             0
#> Y.0001_X1.0_X2.0             0             0             0             0
#> Y.1001_X1.0_X2.0             0             0             0             0
#> Y.0101_X1.0_X2.0             0             0             0             0
#> Y.1101_X1.0_X2.0             0             0             0             0
#> Y.0011_X1.0_X2.0             0             0             0             0
#> Y.1011_X1.0_X2.0             0             0             0             0
#> Y.0111_X1.0_X2.0             0             0             0             0
#> Y.1111_X1.0_X2.0             0             0             0             0
#> Y.0000_X1.0_X2.1             0             0             0             0
#> Y.1000_X1.0_X2.1             0             0             0             0
#> Y.0100_X1.0_X2.1             0             0             0             0
#> Y.1100_X1.0_X2.1             0             0             0             0
#> Y.0010_X1.0_X2.1             0             0             0             0
#> Y.1010_X1.0_X2.1             0             0             0             0
#> Y.0110_X1.0_X2.1             0             0             0             0
#> Y.1110_X1.0_X2.1             0             0             1             0
#> Y.0001_X1.0_X2.1             0             0             0             0
#> Y.1001_X1.0_X2.1             0             0             0             0
#> Y.0101_X1.0_X2.1             0             0             0             0
#> Y.1101_X1.0_X2.1             0             0             0             0
#> Y.0011_X1.0_X2.1             0             0             0             0
#> Y.1011_X1.0_X2.1             0             0             0             0
#> Y.0111_X1.0_X2.1             0             0             0             0
#> Y.1111_X1.0_X2.1             0             0             0             0
#> Y.0000_X1.1_X2.0             0             0             0             0
#> Y.1000_X1.1_X2.0             0             0             0             0
#> Y.0100_X1.1_X2.0             0             0             0             0
#> Y.1100_X1.1_X2.0             0             0             0             0
#> Y.0010_X1.1_X2.0             0             0             0             0
#> Y.1010_X1.1_X2.0             0             0             0             0
#> Y.0110_X1.1_X2.0             0             0             0             0
#> Y.1110_X1.1_X2.0             0             1             0             0
#> Y.0001_X1.1_X2.0             0             0             0             0
#> Y.1001_X1.1_X2.0             0             0             0             0
#> Y.0101_X1.1_X2.0             0             0             0             0
#> Y.1101_X1.1_X2.0             0             0             0             0
#> Y.0011_X1.1_X2.0             0             0             0             0
#> Y.1011_X1.1_X2.0             0             0             0             0
#> Y.0111_X1.1_X2.0             0             0             0             0
#> Y.1111_X1.1_X2.0             0             0             0             0
#> Y.0000_X1.1_X2.1             0             0             0             0
#> Y.1000_X1.1_X2.1             0             0             0             0
#> Y.0100_X1.1_X2.1             0             0             0             0
#> Y.1100_X1.1_X2.1             0             0             0             0
#> Y.0010_X1.1_X2.1             0             0             0             0
#> Y.1010_X1.1_X2.1             0             0             0             0
#> Y.0110_X1.1_X2.1             0             0             0             0
#> Y.1110_X1.1_X2.1             0             0             0             1
#> Y.0001_X1.1_X2.1             0             0             0             0
#> Y.1001_X1.1_X2.1             0             0             0             0
#> Y.0101_X1.1_X2.1             0             0             0             0
#> Y.1101_X1.1_X2.1             0             0             0             0
#> Y.0011_X1.1_X2.1             0             0             0             0
#> Y.1011_X1.1_X2.1             0             0             0             0
#> Y.0111_X1.1_X2.1             0             0             0             0
#> Y.1111_X1.1_X2.1             0             0             0             0
#>                  X10.X20.Y0001 X11.X20.Y0001 X10.X21.Y0001 X11.X21.Y0001
#> X1.0                         1             0             1             0
#> X1.1                         0             1             0             1
#> X2.0                         1             1             0             0
#> X2.1                         0             0             1             1
#> Y.0000_X1.0_X2.0             0             0             0             0
#> Y.1000_X1.0_X2.0             0             0             0             0
#> Y.0100_X1.0_X2.0             0             0             0             0
#> Y.1100_X1.0_X2.0             0             0             0             0
#> Y.0010_X1.0_X2.0             0             0             0             0
#> Y.1010_X1.0_X2.0             0             0             0             0
#> Y.0110_X1.0_X2.0             0             0             0             0
#> Y.1110_X1.0_X2.0             0             0             0             0
#> Y.0001_X1.0_X2.0             1             0             0             0
#> Y.1001_X1.0_X2.0             0             0             0             0
#> Y.0101_X1.0_X2.0             0             0             0             0
#> Y.1101_X1.0_X2.0             0             0             0             0
#> Y.0011_X1.0_X2.0             0             0             0             0
#> Y.1011_X1.0_X2.0             0             0             0             0
#> Y.0111_X1.0_X2.0             0             0             0             0
#> Y.1111_X1.0_X2.0             0             0             0             0
#> Y.0000_X1.0_X2.1             0             0             0             0
#> Y.1000_X1.0_X2.1             0             0             0             0
#> Y.0100_X1.0_X2.1             0             0             0             0
#> Y.1100_X1.0_X2.1             0             0             0             0
#> Y.0010_X1.0_X2.1             0             0             0             0
#> Y.1010_X1.0_X2.1             0             0             0             0
#> Y.0110_X1.0_X2.1             0             0             0             0
#> Y.1110_X1.0_X2.1             0             0             0             0
#> Y.0001_X1.0_X2.1             0             0             1             0
#> Y.1001_X1.0_X2.1             0             0             0             0
#> Y.0101_X1.0_X2.1             0             0             0             0
#> Y.1101_X1.0_X2.1             0             0             0             0
#> Y.0011_X1.0_X2.1             0             0             0             0
#> Y.1011_X1.0_X2.1             0             0             0             0
#> Y.0111_X1.0_X2.1             0             0             0             0
#> Y.1111_X1.0_X2.1             0             0             0             0
#> Y.0000_X1.1_X2.0             0             0             0             0
#> Y.1000_X1.1_X2.0             0             0             0             0
#> Y.0100_X1.1_X2.0             0             0             0             0
#> Y.1100_X1.1_X2.0             0             0             0             0
#> Y.0010_X1.1_X2.0             0             0             0             0
#> Y.1010_X1.1_X2.0             0             0             0             0
#> Y.0110_X1.1_X2.0             0             0             0             0
#> Y.1110_X1.1_X2.0             0             0             0             0
#> Y.0001_X1.1_X2.0             0             1             0             0
#> Y.1001_X1.1_X2.0             0             0             0             0
#> Y.0101_X1.1_X2.0             0             0             0             0
#> Y.1101_X1.1_X2.0             0             0             0             0
#> Y.0011_X1.1_X2.0             0             0             0             0
#> Y.1011_X1.1_X2.0             0             0             0             0
#> Y.0111_X1.1_X2.0             0             0             0             0
#> Y.1111_X1.1_X2.0             0             0             0             0
#> Y.0000_X1.1_X2.1             0             0             0             0
#> Y.1000_X1.1_X2.1             0             0             0             0
#> Y.0100_X1.1_X2.1             0             0             0             0
#> Y.1100_X1.1_X2.1             0             0             0             0
#> Y.0010_X1.1_X2.1             0             0             0             0
#> Y.1010_X1.1_X2.1             0             0             0             0
#> Y.0110_X1.1_X2.1             0             0             0             0
#> Y.1110_X1.1_X2.1             0             0             0             0
#> Y.0001_X1.1_X2.1             0             0             0             1
#> Y.1001_X1.1_X2.1             0             0             0             0
#> Y.0101_X1.1_X2.1             0             0             0             0
#> Y.1101_X1.1_X2.1             0             0             0             0
#> Y.0011_X1.1_X2.1             0             0             0             0
#> Y.1011_X1.1_X2.1             0             0             0             0
#> Y.0111_X1.1_X2.1             0             0             0             0
#> Y.1111_X1.1_X2.1             0             0             0             0
#>                  X10.X20.Y1001 X11.X20.Y1001 X10.X21.Y1001 X11.X21.Y1001
#> X1.0                         1             0             1             0
#> X1.1                         0             1             0             1
#> X2.0                         1             1             0             0
#> X2.1                         0             0             1             1
#> Y.0000_X1.0_X2.0             0             0             0             0
#> Y.1000_X1.0_X2.0             0             0             0             0
#> Y.0100_X1.0_X2.0             0             0             0             0
#> Y.1100_X1.0_X2.0             0             0             0             0
#> Y.0010_X1.0_X2.0             0             0             0             0
#> Y.1010_X1.0_X2.0             0             0             0             0
#> Y.0110_X1.0_X2.0             0             0             0             0
#> Y.1110_X1.0_X2.0             0             0             0             0
#> Y.0001_X1.0_X2.0             0             0             0             0
#> Y.1001_X1.0_X2.0             1             0             0             0
#> Y.0101_X1.0_X2.0             0             0             0             0
#> Y.1101_X1.0_X2.0             0             0             0             0
#> Y.0011_X1.0_X2.0             0             0             0             0
#> Y.1011_X1.0_X2.0             0             0             0             0
#> Y.0111_X1.0_X2.0             0             0             0             0
#> Y.1111_X1.0_X2.0             0             0             0             0
#> Y.0000_X1.0_X2.1             0             0             0             0
#> Y.1000_X1.0_X2.1             0             0             0             0
#> Y.0100_X1.0_X2.1             0             0             0             0
#> Y.1100_X1.0_X2.1             0             0             0             0
#> Y.0010_X1.0_X2.1             0             0             0             0
#> Y.1010_X1.0_X2.1             0             0             0             0
#> Y.0110_X1.0_X2.1             0             0             0             0
#> Y.1110_X1.0_X2.1             0             0             0             0
#> Y.0001_X1.0_X2.1             0             0             0             0
#> Y.1001_X1.0_X2.1             0             0             1             0
#> Y.0101_X1.0_X2.1             0             0             0             0
#> Y.1101_X1.0_X2.1             0             0             0             0
#> Y.0011_X1.0_X2.1             0             0             0             0
#> Y.1011_X1.0_X2.1             0             0             0             0
#> Y.0111_X1.0_X2.1             0             0             0             0
#> Y.1111_X1.0_X2.1             0             0             0             0
#> Y.0000_X1.1_X2.0             0             0             0             0
#> Y.1000_X1.1_X2.0             0             0             0             0
#> Y.0100_X1.1_X2.0             0             0             0             0
#> Y.1100_X1.1_X2.0             0             0             0             0
#> Y.0010_X1.1_X2.0             0             0             0             0
#> Y.1010_X1.1_X2.0             0             0             0             0
#> Y.0110_X1.1_X2.0             0             0             0             0
#> Y.1110_X1.1_X2.0             0             0             0             0
#> Y.0001_X1.1_X2.0             0             0             0             0
#> Y.1001_X1.1_X2.0             0             1             0             0
#> Y.0101_X1.1_X2.0             0             0             0             0
#> Y.1101_X1.1_X2.0             0             0             0             0
#> Y.0011_X1.1_X2.0             0             0             0             0
#> Y.1011_X1.1_X2.0             0             0             0             0
#> Y.0111_X1.1_X2.0             0             0             0             0
#> Y.1111_X1.1_X2.0             0             0             0             0
#> Y.0000_X1.1_X2.1             0             0             0             0
#> Y.1000_X1.1_X2.1             0             0             0             0
#> Y.0100_X1.1_X2.1             0             0             0             0
#> Y.1100_X1.1_X2.1             0             0             0             0
#> Y.0010_X1.1_X2.1             0             0             0             0
#> Y.1010_X1.1_X2.1             0             0             0             0
#> Y.0110_X1.1_X2.1             0             0             0             0
#> Y.1110_X1.1_X2.1             0             0             0             0
#> Y.0001_X1.1_X2.1             0             0             0             0
#> Y.1001_X1.1_X2.1             0             0             0             1
#> Y.0101_X1.1_X2.1             0             0             0             0
#> Y.1101_X1.1_X2.1             0             0             0             0
#> Y.0011_X1.1_X2.1             0             0             0             0
#> Y.1011_X1.1_X2.1             0             0             0             0
#> Y.0111_X1.1_X2.1             0             0             0             0
#> Y.1111_X1.1_X2.1             0             0             0             0
#>                  X10.X20.Y0101 X11.X20.Y0101 X10.X21.Y0101 X11.X21.Y0101
#> X1.0                         1             0             1             0
#> X1.1                         0             1             0             1
#> X2.0                         1             1             0             0
#> X2.1                         0             0             1             1
#> Y.0000_X1.0_X2.0             0             0             0             0
#> Y.1000_X1.0_X2.0             0             0             0             0
#> Y.0100_X1.0_X2.0             0             0             0             0
#> Y.1100_X1.0_X2.0             0             0             0             0
#> Y.0010_X1.0_X2.0             0             0             0             0
#> Y.1010_X1.0_X2.0             0             0             0             0
#> Y.0110_X1.0_X2.0             0             0             0             0
#> Y.1110_X1.0_X2.0             0             0             0             0
#> Y.0001_X1.0_X2.0             0             0             0             0
#> Y.1001_X1.0_X2.0             0             0             0             0
#> Y.0101_X1.0_X2.0             1             0             0             0
#> Y.1101_X1.0_X2.0             0             0             0             0
#> Y.0011_X1.0_X2.0             0             0             0             0
#> Y.1011_X1.0_X2.0             0             0             0             0
#> Y.0111_X1.0_X2.0             0             0             0             0
#> Y.1111_X1.0_X2.0             0             0             0             0
#> Y.0000_X1.0_X2.1             0             0             0             0
#> Y.1000_X1.0_X2.1             0             0             0             0
#> Y.0100_X1.0_X2.1             0             0             0             0
#> Y.1100_X1.0_X2.1             0             0             0             0
#> Y.0010_X1.0_X2.1             0             0             0             0
#> Y.1010_X1.0_X2.1             0             0             0             0
#> Y.0110_X1.0_X2.1             0             0             0             0
#> Y.1110_X1.0_X2.1             0             0             0             0
#> Y.0001_X1.0_X2.1             0             0             0             0
#> Y.1001_X1.0_X2.1             0             0             0             0
#> Y.0101_X1.0_X2.1             0             0             1             0
#> Y.1101_X1.0_X2.1             0             0             0             0
#> Y.0011_X1.0_X2.1             0             0             0             0
#> Y.1011_X1.0_X2.1             0             0             0             0
#> Y.0111_X1.0_X2.1             0             0             0             0
#> Y.1111_X1.0_X2.1             0             0             0             0
#> Y.0000_X1.1_X2.0             0             0             0             0
#> Y.1000_X1.1_X2.0             0             0             0             0
#> Y.0100_X1.1_X2.0             0             0             0             0
#> Y.1100_X1.1_X2.0             0             0             0             0
#> Y.0010_X1.1_X2.0             0             0             0             0
#> Y.1010_X1.1_X2.0             0             0             0             0
#> Y.0110_X1.1_X2.0             0             0             0             0
#> Y.1110_X1.1_X2.0             0             0             0             0
#> Y.0001_X1.1_X2.0             0             0             0             0
#> Y.1001_X1.1_X2.0             0             0             0             0
#> Y.0101_X1.1_X2.0             0             1             0             0
#> Y.1101_X1.1_X2.0             0             0             0             0
#> Y.0011_X1.1_X2.0             0             0             0             0
#> Y.1011_X1.1_X2.0             0             0             0             0
#> Y.0111_X1.1_X2.0             0             0             0             0
#> Y.1111_X1.1_X2.0             0             0             0             0
#> Y.0000_X1.1_X2.1             0             0             0             0
#> Y.1000_X1.1_X2.1             0             0             0             0
#> Y.0100_X1.1_X2.1             0             0             0             0
#> Y.1100_X1.1_X2.1             0             0             0             0
#> Y.0010_X1.1_X2.1             0             0             0             0
#> Y.1010_X1.1_X2.1             0             0             0             0
#> Y.0110_X1.1_X2.1             0             0             0             0
#> Y.1110_X1.1_X2.1             0             0             0             0
#> Y.0001_X1.1_X2.1             0             0             0             0
#> Y.1001_X1.1_X2.1             0             0             0             0
#> Y.0101_X1.1_X2.1             0             0             0             1
#> Y.1101_X1.1_X2.1             0             0             0             0
#> Y.0011_X1.1_X2.1             0             0             0             0
#> Y.1011_X1.1_X2.1             0             0             0             0
#> Y.0111_X1.1_X2.1             0             0             0             0
#> Y.1111_X1.1_X2.1             0             0             0             0
#>                  X10.X20.Y1101 X11.X20.Y1101 X10.X21.Y1101 X11.X21.Y1101
#> X1.0                         1             0             1             0
#> X1.1                         0             1             0             1
#> X2.0                         1             1             0             0
#> X2.1                         0             0             1             1
#> Y.0000_X1.0_X2.0             0             0             0             0
#> Y.1000_X1.0_X2.0             0             0             0             0
#> Y.0100_X1.0_X2.0             0             0             0             0
#> Y.1100_X1.0_X2.0             0             0             0             0
#> Y.0010_X1.0_X2.0             0             0             0             0
#> Y.1010_X1.0_X2.0             0             0             0             0
#> Y.0110_X1.0_X2.0             0             0             0             0
#> Y.1110_X1.0_X2.0             0             0             0             0
#> Y.0001_X1.0_X2.0             0             0             0             0
#> Y.1001_X1.0_X2.0             0             0             0             0
#> Y.0101_X1.0_X2.0             0             0             0             0
#> Y.1101_X1.0_X2.0             1             0             0             0
#> Y.0011_X1.0_X2.0             0             0             0             0
#> Y.1011_X1.0_X2.0             0             0             0             0
#> Y.0111_X1.0_X2.0             0             0             0             0
#> Y.1111_X1.0_X2.0             0             0             0             0
#> Y.0000_X1.0_X2.1             0             0             0             0
#> Y.1000_X1.0_X2.1             0             0             0             0
#> Y.0100_X1.0_X2.1             0             0             0             0
#> Y.1100_X1.0_X2.1             0             0             0             0
#> Y.0010_X1.0_X2.1             0             0             0             0
#> Y.1010_X1.0_X2.1             0             0             0             0
#> Y.0110_X1.0_X2.1             0             0             0             0
#> Y.1110_X1.0_X2.1             0             0             0             0
#> Y.0001_X1.0_X2.1             0             0             0             0
#> Y.1001_X1.0_X2.1             0             0             0             0
#> Y.0101_X1.0_X2.1             0             0             0             0
#> Y.1101_X1.0_X2.1             0             0             1             0
#> Y.0011_X1.0_X2.1             0             0             0             0
#> Y.1011_X1.0_X2.1             0             0             0             0
#> Y.0111_X1.0_X2.1             0             0             0             0
#> Y.1111_X1.0_X2.1             0             0             0             0
#> Y.0000_X1.1_X2.0             0             0             0             0
#> Y.1000_X1.1_X2.0             0             0             0             0
#> Y.0100_X1.1_X2.0             0             0             0             0
#> Y.1100_X1.1_X2.0             0             0             0             0
#> Y.0010_X1.1_X2.0             0             0             0             0
#> Y.1010_X1.1_X2.0             0             0             0             0
#> Y.0110_X1.1_X2.0             0             0             0             0
#> Y.1110_X1.1_X2.0             0             0             0             0
#> Y.0001_X1.1_X2.0             0             0             0             0
#> Y.1001_X1.1_X2.0             0             0             0             0
#> Y.0101_X1.1_X2.0             0             0             0             0
#> Y.1101_X1.1_X2.0             0             1             0             0
#> Y.0011_X1.1_X2.0             0             0             0             0
#> Y.1011_X1.1_X2.0             0             0             0             0
#> Y.0111_X1.1_X2.0             0             0             0             0
#> Y.1111_X1.1_X2.0             0             0             0             0
#> Y.0000_X1.1_X2.1             0             0             0             0
#> Y.1000_X1.1_X2.1             0             0             0             0
#> Y.0100_X1.1_X2.1             0             0             0             0
#> Y.1100_X1.1_X2.1             0             0             0             0
#> Y.0010_X1.1_X2.1             0             0             0             0
#> Y.1010_X1.1_X2.1             0             0             0             0
#> Y.0110_X1.1_X2.1             0             0             0             0
#> Y.1110_X1.1_X2.1             0             0             0             0
#> Y.0001_X1.1_X2.1             0             0             0             0
#> Y.1001_X1.1_X2.1             0             0             0             0
#> Y.0101_X1.1_X2.1             0             0             0             0
#> Y.1101_X1.1_X2.1             0             0             0             1
#> Y.0011_X1.1_X2.1             0             0             0             0
#> Y.1011_X1.1_X2.1             0             0             0             0
#> Y.0111_X1.1_X2.1             0             0             0             0
#> Y.1111_X1.1_X2.1             0             0             0             0
#>                  X10.X20.Y0011 X11.X20.Y0011 X10.X21.Y0011 X11.X21.Y0011
#> X1.0                         1             0             1             0
#> X1.1                         0             1             0             1
#> X2.0                         1             1             0             0
#> X2.1                         0             0             1             1
#> Y.0000_X1.0_X2.0             0             0             0             0
#> Y.1000_X1.0_X2.0             0             0             0             0
#> Y.0100_X1.0_X2.0             0             0             0             0
#> Y.1100_X1.0_X2.0             0             0             0             0
#> Y.0010_X1.0_X2.0             0             0             0             0
#> Y.1010_X1.0_X2.0             0             0             0             0
#> Y.0110_X1.0_X2.0             0             0             0             0
#> Y.1110_X1.0_X2.0             0             0             0             0
#> Y.0001_X1.0_X2.0             0             0             0             0
#> Y.1001_X1.0_X2.0             0             0             0             0
#> Y.0101_X1.0_X2.0             0             0             0             0
#> Y.1101_X1.0_X2.0             0             0             0             0
#> Y.0011_X1.0_X2.0             1             0             0             0
#> Y.1011_X1.0_X2.0             0             0             0             0
#> Y.0111_X1.0_X2.0             0             0             0             0
#> Y.1111_X1.0_X2.0             0             0             0             0
#> Y.0000_X1.0_X2.1             0             0             0             0
#> Y.1000_X1.0_X2.1             0             0             0             0
#> Y.0100_X1.0_X2.1             0             0             0             0
#> Y.1100_X1.0_X2.1             0             0             0             0
#> Y.0010_X1.0_X2.1             0             0             0             0
#> Y.1010_X1.0_X2.1             0             0             0             0
#> Y.0110_X1.0_X2.1             0             0             0             0
#> Y.1110_X1.0_X2.1             0             0             0             0
#> Y.0001_X1.0_X2.1             0             0             0             0
#> Y.1001_X1.0_X2.1             0             0             0             0
#> Y.0101_X1.0_X2.1             0             0             0             0
#> Y.1101_X1.0_X2.1             0             0             0             0
#> Y.0011_X1.0_X2.1             0             0             1             0
#> Y.1011_X1.0_X2.1             0             0             0             0
#> Y.0111_X1.0_X2.1             0             0             0             0
#> Y.1111_X1.0_X2.1             0             0             0             0
#> Y.0000_X1.1_X2.0             0             0             0             0
#> Y.1000_X1.1_X2.0             0             0             0             0
#> Y.0100_X1.1_X2.0             0             0             0             0
#> Y.1100_X1.1_X2.0             0             0             0             0
#> Y.0010_X1.1_X2.0             0             0             0             0
#> Y.1010_X1.1_X2.0             0             0             0             0
#> Y.0110_X1.1_X2.0             0             0             0             0
#> Y.1110_X1.1_X2.0             0             0             0             0
#> Y.0001_X1.1_X2.0             0             0             0             0
#> Y.1001_X1.1_X2.0             0             0             0             0
#> Y.0101_X1.1_X2.0             0             0             0             0
#> Y.1101_X1.1_X2.0             0             0             0             0
#> Y.0011_X1.1_X2.0             0             1             0             0
#> Y.1011_X1.1_X2.0             0             0             0             0
#> Y.0111_X1.1_X2.0             0             0             0             0
#> Y.1111_X1.1_X2.0             0             0             0             0
#> Y.0000_X1.1_X2.1             0             0             0             0
#> Y.1000_X1.1_X2.1             0             0             0             0
#> Y.0100_X1.1_X2.1             0             0             0             0
#> Y.1100_X1.1_X2.1             0             0             0             0
#> Y.0010_X1.1_X2.1             0             0             0             0
#> Y.1010_X1.1_X2.1             0             0             0             0
#> Y.0110_X1.1_X2.1             0             0             0             0
#> Y.1110_X1.1_X2.1             0             0             0             0
#> Y.0001_X1.1_X2.1             0             0             0             0
#> Y.1001_X1.1_X2.1             0             0             0             0
#> Y.0101_X1.1_X2.1             0             0             0             0
#> Y.1101_X1.1_X2.1             0             0             0             0
#> Y.0011_X1.1_X2.1             0             0             0             1
#> Y.1011_X1.1_X2.1             0             0             0             0
#> Y.0111_X1.1_X2.1             0             0             0             0
#> Y.1111_X1.1_X2.1             0             0             0             0
#>                  X10.X20.Y1011 X11.X20.Y1011 X10.X21.Y1011 X11.X21.Y1011
#> X1.0                         1             0             1             0
#> X1.1                         0             1             0             1
#> X2.0                         1             1             0             0
#> X2.1                         0             0             1             1
#> Y.0000_X1.0_X2.0             0             0             0             0
#> Y.1000_X1.0_X2.0             0             0             0             0
#> Y.0100_X1.0_X2.0             0             0             0             0
#> Y.1100_X1.0_X2.0             0             0             0             0
#> Y.0010_X1.0_X2.0             0             0             0             0
#> Y.1010_X1.0_X2.0             0             0             0             0
#> Y.0110_X1.0_X2.0             0             0             0             0
#> Y.1110_X1.0_X2.0             0             0             0             0
#> Y.0001_X1.0_X2.0             0             0             0             0
#> Y.1001_X1.0_X2.0             0             0             0             0
#> Y.0101_X1.0_X2.0             0             0             0             0
#> Y.1101_X1.0_X2.0             0             0             0             0
#> Y.0011_X1.0_X2.0             0             0             0             0
#> Y.1011_X1.0_X2.0             1             0             0             0
#> Y.0111_X1.0_X2.0             0             0             0             0
#> Y.1111_X1.0_X2.0             0             0             0             0
#> Y.0000_X1.0_X2.1             0             0             0             0
#> Y.1000_X1.0_X2.1             0             0             0             0
#> Y.0100_X1.0_X2.1             0             0             0             0
#> Y.1100_X1.0_X2.1             0             0             0             0
#> Y.0010_X1.0_X2.1             0             0             0             0
#> Y.1010_X1.0_X2.1             0             0             0             0
#> Y.0110_X1.0_X2.1             0             0             0             0
#> Y.1110_X1.0_X2.1             0             0             0             0
#> Y.0001_X1.0_X2.1             0             0             0             0
#> Y.1001_X1.0_X2.1             0             0             0             0
#> Y.0101_X1.0_X2.1             0             0             0             0
#> Y.1101_X1.0_X2.1             0             0             0             0
#> Y.0011_X1.0_X2.1             0             0             0             0
#> Y.1011_X1.0_X2.1             0             0             1             0
#> Y.0111_X1.0_X2.1             0             0             0             0
#> Y.1111_X1.0_X2.1             0             0             0             0
#> Y.0000_X1.1_X2.0             0             0             0             0
#> Y.1000_X1.1_X2.0             0             0             0             0
#> Y.0100_X1.1_X2.0             0             0             0             0
#> Y.1100_X1.1_X2.0             0             0             0             0
#> Y.0010_X1.1_X2.0             0             0             0             0
#> Y.1010_X1.1_X2.0             0             0             0             0
#> Y.0110_X1.1_X2.0             0             0             0             0
#> Y.1110_X1.1_X2.0             0             0             0             0
#> Y.0001_X1.1_X2.0             0             0             0             0
#> Y.1001_X1.1_X2.0             0             0             0             0
#> Y.0101_X1.1_X2.0             0             0             0             0
#> Y.1101_X1.1_X2.0             0             0             0             0
#> Y.0011_X1.1_X2.0             0             0             0             0
#> Y.1011_X1.1_X2.0             0             1             0             0
#> Y.0111_X1.1_X2.0             0             0             0             0
#> Y.1111_X1.1_X2.0             0             0             0             0
#> Y.0000_X1.1_X2.1             0             0             0             0
#> Y.1000_X1.1_X2.1             0             0             0             0
#> Y.0100_X1.1_X2.1             0             0             0             0
#> Y.1100_X1.1_X2.1             0             0             0             0
#> Y.0010_X1.1_X2.1             0             0             0             0
#> Y.1010_X1.1_X2.1             0             0             0             0
#> Y.0110_X1.1_X2.1             0             0             0             0
#> Y.1110_X1.1_X2.1             0             0             0             0
#> Y.0001_X1.1_X2.1             0             0             0             0
#> Y.1001_X1.1_X2.1             0             0             0             0
#> Y.0101_X1.1_X2.1             0             0             0             0
#> Y.1101_X1.1_X2.1             0             0             0             0
#> Y.0011_X1.1_X2.1             0             0             0             0
#> Y.1011_X1.1_X2.1             0             0             0             1
#> Y.0111_X1.1_X2.1             0             0             0             0
#> Y.1111_X1.1_X2.1             0             0             0             0
#>                  X10.X20.Y0111 X11.X20.Y0111 X10.X21.Y0111 X11.X21.Y0111
#> X1.0                         1             0             1             0
#> X1.1                         0             1             0             1
#> X2.0                         1             1             0             0
#> X2.1                         0             0             1             1
#> Y.0000_X1.0_X2.0             0             0             0             0
#> Y.1000_X1.0_X2.0             0             0             0             0
#> Y.0100_X1.0_X2.0             0             0             0             0
#> Y.1100_X1.0_X2.0             0             0             0             0
#> Y.0010_X1.0_X2.0             0             0             0             0
#> Y.1010_X1.0_X2.0             0             0             0             0
#> Y.0110_X1.0_X2.0             0             0             0             0
#> Y.1110_X1.0_X2.0             0             0             0             0
#> Y.0001_X1.0_X2.0             0             0             0             0
#> Y.1001_X1.0_X2.0             0             0             0             0
#> Y.0101_X1.0_X2.0             0             0             0             0
#> Y.1101_X1.0_X2.0             0             0             0             0
#> Y.0011_X1.0_X2.0             0             0             0             0
#> Y.1011_X1.0_X2.0             0             0             0             0
#> Y.0111_X1.0_X2.0             1             0             0             0
#> Y.1111_X1.0_X2.0             0             0             0             0
#> Y.0000_X1.0_X2.1             0             0             0             0
#> Y.1000_X1.0_X2.1             0             0             0             0
#> Y.0100_X1.0_X2.1             0             0             0             0
#> Y.1100_X1.0_X2.1             0             0             0             0
#> Y.0010_X1.0_X2.1             0             0             0             0
#> Y.1010_X1.0_X2.1             0             0             0             0
#> Y.0110_X1.0_X2.1             0             0             0             0
#> Y.1110_X1.0_X2.1             0             0             0             0
#> Y.0001_X1.0_X2.1             0             0             0             0
#> Y.1001_X1.0_X2.1             0             0             0             0
#> Y.0101_X1.0_X2.1             0             0             0             0
#> Y.1101_X1.0_X2.1             0             0             0             0
#> Y.0011_X1.0_X2.1             0             0             0             0
#> Y.1011_X1.0_X2.1             0             0             0             0
#> Y.0111_X1.0_X2.1             0             0             1             0
#> Y.1111_X1.0_X2.1             0             0             0             0
#> Y.0000_X1.1_X2.0             0             0             0             0
#> Y.1000_X1.1_X2.0             0             0             0             0
#> Y.0100_X1.1_X2.0             0             0             0             0
#> Y.1100_X1.1_X2.0             0             0             0             0
#> Y.0010_X1.1_X2.0             0             0             0             0
#> Y.1010_X1.1_X2.0             0             0             0             0
#> Y.0110_X1.1_X2.0             0             0             0             0
#> Y.1110_X1.1_X2.0             0             0             0             0
#> Y.0001_X1.1_X2.0             0             0             0             0
#> Y.1001_X1.1_X2.0             0             0             0             0
#> Y.0101_X1.1_X2.0             0             0             0             0
#> Y.1101_X1.1_X2.0             0             0             0             0
#> Y.0011_X1.1_X2.0             0             0             0             0
#> Y.1011_X1.1_X2.0             0             0             0             0
#> Y.0111_X1.1_X2.0             0             1             0             0
#> Y.1111_X1.1_X2.0             0             0             0             0
#> Y.0000_X1.1_X2.1             0             0             0             0
#> Y.1000_X1.1_X2.1             0             0             0             0
#> Y.0100_X1.1_X2.1             0             0             0             0
#> Y.1100_X1.1_X2.1             0             0             0             0
#> Y.0010_X1.1_X2.1             0             0             0             0
#> Y.1010_X1.1_X2.1             0             0             0             0
#> Y.0110_X1.1_X2.1             0             0             0             0
#> Y.1110_X1.1_X2.1             0             0             0             0
#> Y.0001_X1.1_X2.1             0             0             0             0
#> Y.1001_X1.1_X2.1             0             0             0             0
#> Y.0101_X1.1_X2.1             0             0             0             0
#> Y.1101_X1.1_X2.1             0             0             0             0
#> Y.0011_X1.1_X2.1             0             0             0             0
#> Y.1011_X1.1_X2.1             0             0             0             0
#> Y.0111_X1.1_X2.1             0             0             0             1
#> Y.1111_X1.1_X2.1             0             0             0             0
#>                  X10.X20.Y1111 X11.X20.Y1111 X10.X21.Y1111 X11.X21.Y1111
#> X1.0                         1             0             1             0
#> X1.1                         0             1             0             1
#> X2.0                         1             1             0             0
#> X2.1                         0             0             1             1
#> Y.0000_X1.0_X2.0             0             0             0             0
#> Y.1000_X1.0_X2.0             0             0             0             0
#> Y.0100_X1.0_X2.0             0             0             0             0
#> Y.1100_X1.0_X2.0             0             0             0             0
#> Y.0010_X1.0_X2.0             0             0             0             0
#> Y.1010_X1.0_X2.0             0             0             0             0
#> Y.0110_X1.0_X2.0             0             0             0             0
#> Y.1110_X1.0_X2.0             0             0             0             0
#> Y.0001_X1.0_X2.0             0             0             0             0
#> Y.1001_X1.0_X2.0             0             0             0             0
#> Y.0101_X1.0_X2.0             0             0             0             0
#> Y.1101_X1.0_X2.0             0             0             0             0
#> Y.0011_X1.0_X2.0             0             0             0             0
#> Y.1011_X1.0_X2.0             0             0             0             0
#> Y.0111_X1.0_X2.0             0             0             0             0
#> Y.1111_X1.0_X2.0             1             0             0             0
#> Y.0000_X1.0_X2.1             0             0             0             0
#> Y.1000_X1.0_X2.1             0             0             0             0
#> Y.0100_X1.0_X2.1             0             0             0             0
#> Y.1100_X1.0_X2.1             0             0             0             0
#> Y.0010_X1.0_X2.1             0             0             0             0
#> Y.1010_X1.0_X2.1             0             0             0             0
#> Y.0110_X1.0_X2.1             0             0             0             0
#> Y.1110_X1.0_X2.1             0             0             0             0
#> Y.0001_X1.0_X2.1             0             0             0             0
#> Y.1001_X1.0_X2.1             0             0             0             0
#> Y.0101_X1.0_X2.1             0             0             0             0
#> Y.1101_X1.0_X2.1             0             0             0             0
#> Y.0011_X1.0_X2.1             0             0             0             0
#> Y.1011_X1.0_X2.1             0             0             0             0
#> Y.0111_X1.0_X2.1             0             0             0             0
#> Y.1111_X1.0_X2.1             0             0             1             0
#> Y.0000_X1.1_X2.0             0             0             0             0
#> Y.1000_X1.1_X2.0             0             0             0             0
#> Y.0100_X1.1_X2.0             0             0             0             0
#> Y.1100_X1.1_X2.0             0             0             0             0
#> Y.0010_X1.1_X2.0             0             0             0             0
#> Y.1010_X1.1_X2.0             0             0             0             0
#> Y.0110_X1.1_X2.0             0             0             0             0
#> Y.1110_X1.1_X2.0             0             0             0             0
#> Y.0001_X1.1_X2.0             0             0             0             0
#> Y.1001_X1.1_X2.0             0             0             0             0
#> Y.0101_X1.1_X2.0             0             0             0             0
#> Y.1101_X1.1_X2.0             0             0             0             0
#> Y.0011_X1.1_X2.0             0             0             0             0
#> Y.1011_X1.1_X2.0             0             0             0             0
#> Y.0111_X1.1_X2.0             0             0             0             0
#> Y.1111_X1.1_X2.0             0             1             0             0
#> Y.0000_X1.1_X2.1             0             0             0             0
#> Y.1000_X1.1_X2.1             0             0             0             0
#> Y.0100_X1.1_X2.1             0             0             0             0
#> Y.1100_X1.1_X2.1             0             0             0             0
#> Y.0010_X1.1_X2.1             0             0             0             0
#> Y.1010_X1.1_X2.1             0             0             0             0
#> Y.0110_X1.1_X2.1             0             0             0             0
#> Y.1110_X1.1_X2.1             0             0             0             0
#> Y.0001_X1.1_X2.1             0             0             0             0
#> Y.1001_X1.1_X2.1             0             0             0             0
#> Y.0101_X1.1_X2.1             0             0             0             0
#> Y.1101_X1.1_X2.1             0             0             0             0
#> Y.0011_X1.1_X2.1             0             0             0             0
#> Y.1011_X1.1_X2.1             0             0             0             0
#> Y.0111_X1.1_X2.1             0             0             0             0
#> Y.1111_X1.1_X2.1             0             0             0             1
#> [1] 68 64
inspect(model, "parameters_df")
#> 
#> parameters_df
#> Mapping of model parameters to nodal types: 
#> 
#>   param_names: name of parameter
#>   node:        name of endogenous node associated
#>                with the parameter
#>   gen:         partial causal ordering of the
#>                parameter's node
#>   param_set:   parameter groupings forming a simplex
#>   given:       if model has confounding gives
#>                conditioning nodal type
#>   param_value: parameter values
#>   priors:      hyperparameters of the prior
#>                Dirichlet distribution 
#> 
#> 
#> snippet (use grab() to access full 68 x 8 object): 
#> 
#>         param_names node gen   param_set nodal_type      given param_value
#> 1              X1.0   X1   1          X1          0                 0.5000
#> 2              X1.1   X1   1          X1          1                 0.5000
#> 3              X2.0   X2   2          X2          0                 0.5000
#> 4              X2.1   X2   2          X2          1                 0.5000
#> 5  Y.0000_X1.0_X2.0    Y   3 Y.X1.0.X2.0       0000 X1.0, X2.0      0.0625
#> 6  Y.1000_X1.0_X2.0    Y   3 Y.X1.0.X2.0       1000 X1.0, X2.0      0.0625
#> 7  Y.0100_X1.0_X2.0    Y   3 Y.X1.0.X2.0       0100 X1.0, X2.0      0.0625
#> 8  Y.1100_X1.0_X2.0    Y   3 Y.X1.0.X2.0       1100 X1.0, X2.0      0.0625
#> 9  Y.0010_X1.0_X2.0    Y   3 Y.X1.0.X2.0       0010 X1.0, X2.0      0.0625
#> 10 Y.1010_X1.0_X2.0    Y   3 Y.X1.0.X2.0       1010 X1.0, X2.0      0.0625
#>    priors
#> 1       1
#> 2       1
#> 3       1
#> 4       1
#> 5       1
#> 6       1
#> 7       1
#> 8       1
#> 9       1
#> 10      1

# A single node graph is also possible
model <- make_model("X")

# Unconnected nodes not allowed
if (FALSE) { # \dontrun{
 model <- make_model("X <-> Y")
} # }

# ---- Large / many-parent models ------------------------------------------

# Preferred: reduce types at construction (no 65k table for 4 parents)
m_bare <- make_model(
  "A -> Y <- B; C -> Y; D -> Y",
  drop_interactions = TRUE,
  monotone = "+",
  legacy = FALSE
)
#> Y: kept 6 types (saturated would be 65,536)
length(m_bare$nodal_types$Y)
#> [1] 6
nrow(m_bare$parameters_df)
#> [1] 14

# Default legacy = FALSE: wider DAG without attaching causal_types
big <- make_model("A -> M -> Y; B -> Y; C -> Y", legacy = FALSE)
big$causal_types  # NULL
#> NULL

# Or thin after the fact
make_model("A -> Y <- B; C -> Y") |>
  simplify_model(drop_interactions = 2, monotone = "+") |>
  inspect("nodal_types")
#> Y: kept 5 types (saturated would be 256)
#> 
#> nodal_types (Nodal types): 
#> $A
#> 0  1
#> 
#> NULL
#> 
#> $B
#> 0  1
#> 
#> NULL
#> 
#> $C
#> 0  1
#> 
#> NULL
#> 
#> $Y
#> 00000000  11111111  01010101  00110011  00001111
#> 
#>      index                  interpretation
#> 1 *-------  Y = * if A = 0 & B = 0 & C = 0
#> 2 -*------  Y = * if A = 1 & B = 0 & C = 0
#> 3 --*-----  Y = * if A = 0 & B = 1 & C = 0
#> 4 ---*----  Y = * if A = 1 & B = 1 & C = 0
#> 5 ----*---  Y = * if A = 0 & B = 0 & C = 1
#> 6 -----*--  Y = * if A = 1 & B = 0 & C = 1
#> 7 ------*-  Y = * if A = 0 & B = 1 & C = 1
#> 8 -------*  Y = * if A = 1 & B = 1 & C = 1
#> 
#> 
#> Number of types by node:
#> A B C Y 
#> 2 2 2 5 

# Hand-specified types still work (e.g. five parents)
pa5 <- c("A", "B", "C", "D", "E")
assign5 <- as.matrix(expand.grid(lapply(pa5, function(p) 0:1)))[, pa5, drop = FALSE]
#> Error in as.matrix(expand.grid(lapply(pa5, function(p) 0:1)))[, pa5, drop = FALSE]: subscript out of bounds
nt5 <- c(
  setNames(lapply(pa5, function(p) c("0", "1")), pa5),
  list(Y = c(
    paste(rep(0, nrow(assign5)), collapse = ""),
    paste(rep(1, nrow(assign5)), collapse = ""),
    paste(assign5[, 1], collapse = "")
  ))
)
#> Error: object 'assign5' not found
make_model("A -> Y; B -> Y; C -> Y; D -> Y; E -> Y",
           nodal_types = nt5) |>
  inspect("parameters_df")
#> Error: object 'nt5' not found

make_model("Z -> Y", nodal_types = list(Z = c("0", "1"), Y = c("01", "10"))) |>
  inspect("parameters_df")
#> 
#> parameters_df
#> Mapping of model parameters to nodal types: 
#> 
#>   param_names: name of parameter
#>   node:        name of endogenous node associated
#>                with the parameter
#>   gen:         partial causal ordering of the
#>                parameter's node
#>   param_set:   parameter groupings forming a simplex
#>   given:       if model has confounding gives
#>                conditioning nodal type
#>   param_value: parameter values
#>   priors:      hyperparameters of the prior
#>                Dirichlet distribution 
#> 
#>   param_names node gen param_set nodal_type given param_value priors
#> 1         Z.0    Z   1         Z          0               0.5      1
#> 2         Z.1    Z   1         Z          1               0.5      1
#> 3        Y.01    Y   2         Y         01               0.5      1
#> 4        Y.10    Y   2         Y         10               0.5      1

if (FALSE) { # \dontrun{
# Saturated four-parent Y (65,536 types) — avoid unless you mean it
make_model("A -> Y <- B; C -> Y; D -> Y")

# Legacy path with a huge causal-type *product*: allow_large = TRUE
make_model("A -> Y <- B; C -> Y; D -> Y", legacy = TRUE, allow_large = TRUE)
} # }
```
