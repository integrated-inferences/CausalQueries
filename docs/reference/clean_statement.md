# Helper to clean and check the validity of causal statements specifying a DAG. This function isolates nodes and edges specified in a causal statements and makes them processable by `make_dag`

Helper to clean and check the validity of causal statements specifying a
DAG. This function isolates nodes and edges specified in a causal
statements and makes them processable by `make_dag`

## Usage

``` r
clean_statement(statement)
```

## Arguments

- statement:

  character string. Statement describing causal relations between nodes.

## Value

a list of nodes and edges specified in the input statement
