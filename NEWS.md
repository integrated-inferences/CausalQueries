# CausalQueries 1.5.0

**AI statement.** 1.5 is the first version of CausalQueries with major support
from AI models for refactoring, including new AI-written code and documentation.
The codebase and documentation build on architecture developed by humans in
earlier versions of CausalQueries, and humans also generated multiple tests for
all major parts of the codebase. So while we have reviewed the codebase for 1.5,
we did not write it all. The guarantee offered for the package, then, is not
that every line of code has been vouched for. Rather, it is that the package
can be shown, through repeated testing, to do what it says it does.

The major changes since the CRAN 1.4.x line (behavioural baseline: causal-type
workflow through 1.4.6) are as follows.

## Speed and scale

### Factorized default (parameters-only path)

`make_model()`, `update_model()`, and `query_*` gain a `legacy` argument
(default `FALSE`, overridable via `options(CausalQueries.legacy)`).

* `legacy = FALSE` (default): parameters-only / factorized path. Update uses
  Stan model `simplexes_factorized` without a causal-type matrix `P`
  (unconfounded and confounded via path-encoded `parmap`). Query uses
  relevant-set variable elimination with stratified weights under `<->`.
  Fitted models are stamped so later steps inherit the method. Usual workflows
  (`query_model` / `query_distribution`, `update_model`, `make_data`,
  `get_event_probabilities`, `realise_outcomes`, `inspect` / `grab`) still
  work; large structural objects are built **on demand**, not stored on the
  model by default.
* `legacy = TRUE`: restores the previous causal-type Stan and query path,
  including attaching causal types at `make_model` and optional
  `type_posterior` when `keep_type_distribution = TRUE`.

See vignette `g-factorized-path` (source `vignettes/g-factorized-path.Rmd.orig`).

Answers for supported models and queries match the legacy path within MCMC /
floating-point noise. The asymptotic win is avoiding the global causal-type
**product** for ordinary update and query on modular DAGs; wall-clock gains on
tiny vignette-sized graphs are modest and noisy.

### Shrinking nodal types

`make_model()` gains `drop_interactions`, `keep_interactions`, and `monotone`
to build reduced nodal-type sets at construction (e.g. four parents without
65,536 schedules). `simplify_model()` (alias `set_nodal_restrictions()`)
applies the same rules to an existing model. Monotone codes are `+` / `-`
(weakly increasing / decreasing in a parent), `m` (no qualitative interaction:
effect of that parent never sign-changes across backgrounds), and `n` (keep
only sign-changing / qualitative-interaction types). Prefer
`monotone = list(Y = c(A = "m", B = "+"))` or board-wide `monotone = "m"` /
`"+"`. See `?simplify_model`.

### Size guards

* `allow_large`: when the causal-type product exceeds one million and types
  will be built, `make_model` errors unless `allow_large = TRUE` (then warns).
  `add_causal_types = FALSE` skips the check. Restricted `nodal_types` are
  assessed by actual lengths.
* Auto-generating nodal types for a node with five or more parents is refused;
  pass `nodal_types` explicitly.
* Factorized Stan prep is capped by `options(CausalQueries.factorized_grid_max)`
  (default 4096). Coarsened / missing-data VE on the R side is available
  (`prob_event_ve`).

### Performance hygiene

Internal R paths that recomputed the same objects, or used nested `apply` /
dplyr where a single matrix call suffices, have been rewritten. Answers are
unchanged. These are cleanliness / asymptotic improvements, not a claimed
user-facing speedup for typical vignette-sized DAGs.

* `query_model()` subsets the type-probability matrix once per estimand;
  `colSums` in place of `apply(..., 2, sum)`.
* `set_confound()` / `set_restrictions()` / `get_event_probabilities()` use
  `rowSums` / `rowsum` where appropriate.
* `prep_stan_data()` builds data families once; `get_data_families()` builds
  `E` with one matrix multiply (Stan still `w_full = E * w`).
* Query-path `realise_outcomes` memoized by `dos` (`model$.cache`, cleared by
  mutators); leaner C++ type-probability paths; leaner default `stan_summary`
  (`lambdas` + `lp__` unless event/type draws are retained).

## Fundamentals (API)

### Non-backwards-compatible defaults

* **Default path is factorized** (`legacy = FALSE`). Scripts that assumed the
  old causal-type default without setting `legacy` now use the factorized
  path.
* **`model$causal_types` and `model$P` are `NULL` by default** after
  `make_model()`. Use `grab` / `inspect` rather than raw slots.
* **No `type_posterior` on factorized updates.** Even with
  `keep_type_distribution = TRUE`, factorized fits do not store type draws.
  Use `posterior_distribution` and `query_model(..., using = "posteriors")`,
  or re-update with `legacy = TRUE` and `keep_type_distribution = TRUE`.
* **Leaner default `stan_summary`:** printed Stan parameters are `lambdas`
  and `lp__` unless event/type draws were retained.

### Confounding and missing data

Unobserved confounding (`<->`) is supported on the factorized path (confound
blocks; stratified parameters). Likelihood contract unchanged in spirit:
`w` on complete data types, `w_full = E * w` for observed / coarsened events.

## Robustness and correctness

* `update_model()` respects user `control` (`adapt_delta`, `max_treedepth`,
  `save_warmup`); errors on invalid `censored_types`; checks `parameters_df`
  contiguity by `param_set` / `node`.
* `realise_outcomes()` rejects `dos` other than 0/1 (R and C++).
* `set_restrictions()`: warn and return unchanged when nothing matches; error
  if a node would lose all nodal types. `&` inside do-brackets errors with a
  comma hint (no auto-rewrite).
* `get_event_probabilities(given = )` conditions on possible events (rows of
  `w`), not the full \(2^n\) grid after restrictions.
* `collapse_data()`: warn with drop count for inconsistent 0/1 coding; error
  if every row is inconsistent. `set_confound()` uses exact token match (not
  substring `grepl`); `clean_statement` requires `make.names(x) == x`.
* Default `query_model()` stats use `na.rm = TRUE` for mean, sd, and credible
  bounds. Query evaluation runs in a child environment so node names cannot
  collide with function locals.
* `inspect()` / `grab()`: missing `what` OK; vector `what` returns a named
  list; shared catalogue of component names with `summary` /
  `print.summary`. `summary()` on a `model_query` returns class
  `summary.model_query`.
* `make_data()` / observe-data fixes for `probs`, subsets, and oversampling;
  informative errors when no query is supplied to `query_*`;
  `make_parameters(..., param_type = "posterior_*")` uses `has_posterior()`.

## Interface, docs, and packaging

* `plot.model_query`: explicit error-bar whisker width (ggplot2 4 default was
  too tall and merged adjacent rows).
* `plot_model`: layout normalization, confound-arc controls, modest pad /
  margins, `clip = "off"`; panels fill the device (`coord_cartesian`).
* Vignettes: edit `vignettes/<name>.Rmd.orig`, rebuild with
  `CausalQueries:::build_vignettes()`; figures under `vignettes/figures/<name>/`.
* Documentation: arrow syntax (not "dagitty"); clearer `type_posterior` vs
  `keep_type_distribution`; `w` vs `w_full`; JSS citation; pkgdown reference
  index grouped by task (make / inspect / update / query / data / helpers).
* Package hex / favicons; print/summary methods split across
  `methods_causal_model.R` and `methods_model_query.R`.

In addition: documentation fixes and corrections to declared dependencies.
The package attach message again prints a copy-paste command for setting
`options(mc.cores = parallel::detectCores())` when `mc.cores` is unset.

# CausalQueries 1.4.6

This is a documentation-only release adding the citation for the Journal of
Statistical Software paper (Tietz, Medina, Syunyaev and Humphreys 2026).

# CausalQueries 1.4.5

This patch release reverts changes made to the main Stan model in 1.4.4 which 
introduced a bug when updating models with multiple data strategies.
This patch release additionally reintroduces linking to BH as dropping this package
introduced compilation issues on some systems.

# CausalQueries 1.4.4

This patch release includes an improved Stan model that implements:

* Memory Efficiency: Eliminated large intermediate matrices (parlam, parlam2)
* Numerical Stability: Added safeguards against log(0) with + 1e-10
* Better Documentation: Clear section headers and comprehensive comments
* Error Handling: Validation checks for edge cases
* Mathematical Equivalence: Verified to produce identical results 
  (within floating-point precision)

The core of these edits were suggested by cursor and verified by 
the development team.

In addition:

* Startup advice on Automatic Parallel Computation
* Better Error Messages: Added helpful warning messages for common query syntax mistakes (e.g., using & instead of , in conditions) with auto-correction
* Removed dependency on latex2exp
* Allow expressions for labels in plot_model. See examples.


# CausalQueries 1.4.3

This is a minor release implementing more intuitive nodal type interpretations 
as well as improved warnings around inadmissible model and query specifications 
to guard against silent undefined behavior when querying models. 

### Non Backwards Compatible Changes
`make_model()` now throws an error if node names contain substrings matching
non-linear mathematical transformations (`log(`, `exp(`, `^`, `\`) or `CausalQueries`
query operators (`[`, `]`, `:|:`). This guards against silent undefined behavior 
when parsing and evaluating queries. 

### New Functionality

#### 1. Warnings for unsupported queries
Query related functions now throw a warning when non-linear transformations 
(`log(`, `exp(`, `^`, `\`) are specified; as non-linear queries are not 
currently supported by `CausalQueries`. Previously non-linear queries would 
silently return `NaN` or `Inf`.  

#### 2. More intuitive nodal type interpretations
`summary.causal_model()` and `inspect(model, "nodal_types")` now print a more 
intuitive nodal type interpretation guide. For a model `X -> Y <- Z` the updated
interpretation guide looks as follows: 

```
  index          interpretation
1  *---  Y = * if X = 0 & Z = 0
2  -*--  Y = * if X = 1 & Z = 0
3  --*-  Y = * if X = 0 & Z = 1
4  ---*  Y = * if X = 1 & Z = 1
```

# CausalQueries 1.3.3

This is a patch release implementing Stan optimization improving run time and 
output formatting of the core Stan model. Additionally `type_distribution` has
been renamed to `type_posterior` for clarity. See `grab()` or `inspect()` 
documentation for details. 

# CausalQueries 1.3.2

This is a patch release updating documentation and removing the deprecated 
`causal_type_query` and `nodal_type_query` classes with their associated S3
print methods. 

### Non Backwards Compatible Changes 
The `causal_type_query` and `nodal_type_query` with their associated S3 print 
methods `print.causal_type_query` and `print.nodal_type_query` have 
been removed. `nodal_types` and `causal_types` implicated by queries can
still be inspected via the `list` output of `map_query_to_causal_type()` and
`map_query_to_nodal_type()`.
This deprecation supports our efforts to reduce complexity and improve usability 
by centering methods around the core `causal_model` and `model_query` classes, 
facilitating the make, update, query workflow of `CausalQueries`.

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

Calls to `sapply()` have been replaced with `vapply()` wherever possible to 
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
