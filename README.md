# gbiqq

`qgbiqq` is a package that lets you declare binary causal models, update beliefs about causal types given data and calculate arbitrary estimands.


## Causal models

Causal models are defined by:

* A directed acyclic graph (DAG), which provides the set of (binary) variables, a causal ordering between them, and a set of assumptions regarding conditional independence. If there is no arrow from A to B then a change in A never induces a change in B. 
* Reductions on causal types. The DAG implies a set of "causal types." Units are classed together as of the same causal type if they respond to the same way to other variables.  For instance a type might be the set of units for which $X=1$ and for which $Y=1$ if and only if $X=1$. The set of causal types grows rapidly with the number of nodes and the number of nodes pointing into any given node. Restrictions on causal types can reduce that complexity but require substantive assumptions. An example of a restriction might be "$Y$ is monotonic in $X$."  
