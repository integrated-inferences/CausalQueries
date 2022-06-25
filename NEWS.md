# CausalQueries 0.1.0

This is a minor release introducing new and simplified functionality. 

## New functionality 

* `plot_dag()`: improved `ggplot2` based plotting of model DAGs
* `set_priors()`, `set_parameters()`: parameter sets can now be specified via `node`, `nodal_type`, `label`, `param_set`, `given`, `statement` or a logical `alter_at` statement. 
* `set_restrictions()`: nodal types can now be restricted using `given` statements. 

## Changed functionality

* setting confounds using lists of statements has been discontinued. Confounding is now specified via `<->` in `make_model()` or `set_confounds()`
