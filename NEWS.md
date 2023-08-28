# CausalQueries 0.1.1

This is a patch release fixing documentation issues and a bug in `realise_outcomes()`. 

## Bug fix

* specifying the `node` option in `realise_outcomes()` for a multi-parent nodes without specifying `dos` for all parents previously led to undefined behavior.  The updated function now throws an error. 
