# CausalQueries 1.0.0

### Non Backwards Compatible Changes 

`query_distribution()` now supports the use of multiple queries in one function call and thus returns a `DataFrame`
of distribution draws instead of a single numeric vector.   

### New Functionality   
#### Querying   

`query_distribution()`: now supports the specification of multiple queries and givens to be evaluated on a single model in one function call. 

```
 model <- make_model("X -> Y")
 
 query_distribution(model,
   query = list("(Y[X=1] > Y[X=0])", "(Y[X=1] < Y[X=0])"),
   given = list("Y==1", "(Y[X=1] <= Y[X=0])"),
   using = "priors")|>
 head()
```

`query_model()`: now supports the specification of multiple models to evaluate a set of queries on in one function call. 

```
 models <- list(
  M1 = make_model("X -> Y"),
  M2 = make_model("X -> Y") |> set_restrictions("Y[X=1] < Y[X=0]")
  )
  
 query_model(
  models,
  query = list(ATE = "Y[X=1] - Y[X=0]", Share_positive = "Y[X=1] > Y[X=0]"),
  given = c(TRUE,  "Y==1 & X==1"),
  using = c("parameters", "priors"),
  expand_grid = FALSE)

 query_model(
  models,
  query = list(ATE = "Y[X=1] - Y[X=0]", Share_positive = "Y[X=1] > Y[X=0]"),
  given = c(TRUE,  "Y==1 & X==1"),
  using = c("parameters", "priors"),
  expand_grid = TRUE)
```

This eliminates the need for redundant function calls when querying models and substantially improves computation time 
as computationally expensive function calls to produce data structures required for querying are now reduced to a minimum via redundancy elimination and caching. 


#### Realising Outcomes and Interpreting Nodal-/Causal-Types 

`realise_outcomes()`: specifying the `node` option now produces a `DataFrame` detailing how the specified node responds to its parents in the presence or absence of do operations. This produces a reduced form of the usual `realise_outcomes()` output detailing all causal-types; and aids in the interpretation of both nodal- and causal-types. This update resolves previous bugs and errors relating to specification of nodes with multiple parents in the `node` option. 

```
 model <- make_model("X1 -> M -> Y -> Z; X2 -> Y") |>
  realise_outcomes(dos = list(M = 1), node = "Y") 
```

### Bug Fixes

#### 1. Setting Parameters and Priors

Previously `set_parameters()` and `set_priors()` would default applying changes in the order in which parameters appeared in the `parameters_df` `DataFrame`; regardless of the order in which changes were specified in the aforementioned functions. 
Calling: 

```
 model <- make_model("X -> Y")
 set_priors(model, alphas = c(3,4), nodal_type = c("10",00))
```

would results in the following `parameters_df`.

```
  param_names node    gen param_set nodal_type given param_value priors
  <chr>       <chr> <int> <chr>     <chr>      <chr>       <dbl>  <dbl>
1 X.0         X         1 X         0          ""           0.5       1
2 X.1         X         1 X         1          ""           0.5       1
3 Y.00        Y         2 Y         00         ""           0.25      3
4 Y.10        Y         2 Y         10         ""           0.25      4
5 Y.01        Y         2 Y         01         ""           0.25      1
6 Y.11        Y         2 Y         11         ""           0.25      1
```

Now changes to parameters values get applied in the order specified in the function call; resulting in the following `parameters_df` for the above example:

```
  param_names node    gen param_set nodal_type given param_value priors
  <chr>       <chr> <int> <chr>     <chr>      <chr>       <dbl>  <dbl>
1 X.0         X         1 X         0          ""           0.5       1
2 X.1         X         1 X         1          ""           0.5       1
3 Y.00        Y         2 Y         00         ""           0.25      4
4 Y.10        Y         2 Y         10         ""           0.25      3
5 Y.01        Y         2 Y         01         ""           0.25      1
6 Y.11        Y         2 Y         11         ""           0.25      1
```

Additionally we have implemented helpful warnings for when instructions identifying parameters to be updated are under specified. This is particularly useful when setting priors or parameters on models with confounding as changes may inadvertently be applied across `param_sets`.    


#### 2. Updating with Censored Types 

Previously updating models with censored types would fail as 0s in the `w` vector induced by censoring would evaluate to -Inf as the `Stan` MCMC algorithm began sampling from the posterior of the multinational distribution.
We resolved this issue by pruning the `w` vector when the multinomial is run. This preserves the true
`w` vector (event probabilities without censoring) while still updating with the censored data-


#### 3. Setting Restrictions with Wild Cards 

Previously `wildcards` in `set_restrictions()` were erroneously interpreted as valid nodal types, leading to errors and undefined behavior. Proper unpacking and mapping of `wildcards` to existing nodal types has been restored. 


#### 4. Allowing overwriting of a Parameter Matrix 

Previously a parameter matrix `P` that was attached to a `causal_model` object could not be overwritten. Overwrites are now possible.   


### Improvements 

#### 1. Fast `realise_outcomes()`

We achieved a ~100 fold speed gain in the `realise_outcomes()` functionality. Nodal types on a given node are generated as the Cartesian product of parent realizations. Consider the meaning of nodal types on a node $Y$ with 3 parents $[X1,X2,X3]$:

| X1   | X2   | X3   |
|------|------|------|
|  0   |  0   |  0   |
|  1   |  0   |  0   |
|  0   |  1   |  0   |
|  1   |  1   |  0   |
|  0   |  0   |  1   |
|  1   |  0   |  1   |
|  0   |  1   |  1   |
|  1   |  1   |  1   |

The first digit of each nodal type of $Y$ (see first row above), corresponds to the realization of $Y$ when $X1 = 0, X2 = 0, X3 = 0$. The fourth digit of each nodal type of $Y$ (see fourth row above), corresponds to the realization of $Y$ when $X1 = 1, X2 = 1, X3 = 0$. Finding the position of the realization value of $Y$ in a nodal type given parent realizations is equivalent to finding the row number in the Cartesian product `DataFrame`. By definition of the Cartesian product the number of consecutive 0 or 1 elements in a given column is $2^{columnindex - 1}$. Given a vector of parent realizations $R$ indexed from 0 the corresponding row number can this be computed via $row = 1 + (\sum_{i = 0}^{|R| - 1} (2^{i} \times R_i))$. We implement a fast `C++` version of this computing powers of 2 via bit shifting. 

#### 2. `Stan` update

We updated to the new array syntax introduced in `Stan` `v2.33.0`
