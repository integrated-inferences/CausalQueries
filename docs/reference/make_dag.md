# Helper to run a causal statement specifying a DAG into a `data.frame` of pairwise parent child relations between nodes specified by a respective edge.

Helper to run a causal statement specifying a DAG into a `data.frame` of
pairwise parent child relations between nodes specified by a respective
edge.

## Usage

``` r
make_dag(statement)
```

## Arguments

- statement:

  character string. Statement describing causal relations between nodes.
  Only directed relations are permitted. For instance "X -\> Y" or "X1
  -\> Y \<- X2; X1 -\> X2"

## Value

a `data.frame` with columns v, w, e specifying parent, child and edge
respectively
