# CausalQueries 1.3.1

This is a patch release fixing a labeling bug in the `model_query` class S3
plot method. Please refer to the `1.3.0` release note + news for the most recent
functionality updates. 


# CausalQueries 1.3.0 

This is a minor release introducing the option to specify causal queries with
givens in a single statement. This new functionality is meant to make query 
specification more concise, expressive, and intuitive for users more comfortable 
with standard statistical notation for conditional distributions.

### New Functionality 

#### 1. Combining queries and givens 
Instead of specifying the conditioning set of a query in the `given` argument 
the `given` statement defining the conditioning set may now be added to the
query statement directly after the `:|:` operator. We opt for `:|:` instead of 
the traditional `|` conditioning operator to avoid confusion with the built in 
logical or operator `|`. 

```
model <- CausalQueries::make_model("X -> Y")

# using given argument 
CausalQueries::query_model(model, queries = "Y[X=1] - Y[X=0]", given = "X == 1 & Y == 1")

# new combined specification option
CausalQueries::query_model(model, queries = "Y[X=1] - Y[X=0] :|: X == 1 & Y == 1")
```

# CausalQueries 1.2.1

This is a minor release introducing changes meant to focus S3 methods and 
utility functions around two core classes: `causal_model` and `model_query`. 
Our aim is to improve the user experience of `CausalQueries` by focusing 
user facing functionality more clearly around the workflow of making, updating,
querying and inspecting causal models. 
With respect to `causal_model` objects this release introduces more expressive 
and concise S3 summary and print methods for the `causal_model` class and its 
internal objects. Updates to the `grab()` and `inspect()` functions streamline
access to objects contained within a `causal_model`, facilitating more advanced
use-cases or deeper review.
This release introduces the `model_query` class along with S3 summary, print and 
plot methods for a more seamless querying workflow. 
Finally, this release removes dependency on `dagitty`, restoring compatibility of
`CausalQueries` with systems on which `V8` `JavaScript` `WASM` is not supported. 

### New Functionality

#### 1. Improved causal_model summaries
The `summary()` method for objects of class `causal_model` now supports an
`include` argument allowing users to specify additional objects internal 
to the `causal_model` object for which they would like to have summaries 
appended to the main output of `summary()`. Summaries have additionally been 
made more informative and readable. Please see `?summary.causal_model` for 
extensive documentation on the new functionality.

#### 2. Streamlined causal_model object access
Internal objects of a `causal_model` instance can now be returned quietly 
via `grab()` eliminating the need to interact with a `causal_model` instance
directly. 

#### 3. New querying utility functionality
The newly introduced `model_query` class comes with a print, summary and plot
method. `plot()` generates a coefficient plot with credible intervals for 
evaluated queries. 

# CausalQueries 1.1.1

This is a patch release fixing a bug in the `print.model_query()` S3 method that 
occurred when querying models using `paramters`.

# CausalQueries 1.1.0

### Non Backwards Compatible Changes 

Accessing `causal-model` objects via `get_` methods e.g. `get_nodal_types()`, `get_parameters` is no longer supported. Objects may now be accessed via a unified syntax through the `inspect()` function (see New Functionality). 
The following functions are no longer exported: 

- `get_causal_types()`
- `get_nodal_types()`
- `get_all_data_types()`
- `get_event_probabilities()`
- `get_ambiguities_matrix()`
- `get_parameters()`
- `get_parameter_names()`
- `get_parmap()`
- `get_parameter_matrix()`
- `get_priors()`
- `get_param_dist()`
- `get_type_prob_multiple()`

### New Functionality

#### 1. unified object access syntax via `inspect()`

`causal-model` objects can now be accessed via `inspect()` like so: 

```
inspect(model, "parameters_df")
```

See documentation for an exhaustive list of accessible objects. `causal-model` objects now additionally come with dedicated `print` methods returning short informative summaries of the given object.

#### 2. model diagnostics

A summary of parameter values and convergence information produced by the `update_model()` `Stan` model can now be accessed via:

```
inspect(model, "stan_summary")
```

Advanced model diagnostics on raw `Stan` output via external packages is possible by saving the `stan_fit` object when updating. This is facilitated via the `keep_fit` option in `update_model()`: 

```
model <- make_model("X -> Y") |> 
  update_model(data, keep_fit = TRUE)
  
model |> inspect("stanfit")
```


# CausalQueries 1.0.2

### Bug Fixes 

#### 1. passing `nodal_types` to `make_model()` now implements correct error handling 

Previously this `make_model("X -> Y" , nodal_types = list(Y = c("0", "1")))` was
permissible leading to setting `nodal_types`:

```
$X
NULL

$Y
[1] "0" "1"
```

This led to undefined behavior and unhelpful downstream error messages. 
When passing `nodal_types` to `make_model()` users are now forced to specify 
a set of `nodal_types` on each node.  


#### 2. `query_distribution()` are no longer overwrites type distribution internally  


#### 3. node naming checks are operational in `make_model()`  

Previously hyphenated names would not throw an error and be corrupted 
silently through the conversion of model definition strings into 
`dagitty` objects.

```
make_model("institutions -> political-inequality")

Statement: 
[1] "institutions -> political-inequality"

DAG: 
        parent  children
1 institutions political
```

Checks for correct variable naming are now reinstated. 

### Improvements

#### 1. type safety 

Calls to `sapply()` have ben replaced with `vapply()` wherever possible to 
enforce type safety.   


#### 2. range based looping 

Looping via index has been replaced by range based looping wherever possible 
to guard against 0 length exceptions.  


#### 3. `goodpractice::gp()`

`goodpractice` code improvements have been implemented.


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


#### 4. Checks for Misspecified Queries

Previously misspecifications in queries like `Y[X==1]=1` would lead to undefined behavior when mapping queries to nodal or causal types. We now correct misspecified queries internally and warn about the misspecification. For example; running:

```
model <- CausalQueries::make_model("X -> Y")
get_query_types(model, "Y[X=1]=1")
```

now produces

```
Causal types satisfying query's condition(s)  

 query =  Y[X=1]==1 

X0.Y01  X1.Y01
X0.Y11  X1.Y11


 Number of causal types that meet condition(s) =  4
 Total number of causal types in model =  8
Warning message:
In check_query(query) :
  statements to the effect that the realization of a node should equal some value should be specified with `==` not `=`. 
  The query has been changed accordingly: Y[X=1]==1

```


#### 5. Allowing overwriting of a Parameter Matrix 

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

Each row in the above `DataFrame` corresponds to a digit in `Y's` nodal types. The first digit of each nodal type of $Y$ (see first row above), corresponds to the realization of $Y$ when $X1 = 0, X2 = 0, X3 = 0$. The fourth digit of each nodal type of $Y$ (see fourth row above), corresponds to the realization of $Y$ when $X1 = 1, X2 = 1, X3 = 0$. Finding the position of the realization value of $Y$ in a nodal type given parent realizations is equivalent to finding the row number in the Cartesian product `DataFrame`. By definition of the Cartesian product, the number of consecutive 0 or 1 elements in a given column is $2^{columnindex}$, when indexing columns from 0. Given a set of parent realizations $R$ indexed from 0, the corresponding row in a number in a `DataFrame` indexed from 0 can thus be computed via:
$$row = (\sum_{i = 0}^{|R| - 1} (2^{i} \times R_i))$$. 
We implement a fast `C++` version of this computing powers of 2 via bit shifting. 

#### 2. `Stan` update

We updated to the new array syntax introduced in `Stan` `v2.33.0`
