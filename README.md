# CausalQueries

<!-- badges: start -->
[![CRAN status](https://www.r-pkg.org/badges/version/CausalQueries)](https://CRAN.R-project.org/package=CausalQueries)
![](https://cranlogs.r-pkg.org/badges/grand-total/CausalQueries)
[![Lifecycle:Stable](https://img.shields.io/badge/Lifecycle-Stable-97ca00)](<Redirect-URL>)
<!-- badges: end -->


`CausalQueries` is a package that lets you declare binary causal models, update beliefs about causal types given data and calculate arbitrary estimands. Model definition makes use of `dagitty` functionality. Updating is implemented in `stan`. 

* See vignettes for a guide to getting started.

* See [here](https://macartan.github.io/causalmodels/) for a guide to using `CausalQueries` along with many examples of causal models  

 

## Installation

To install the latest stable release of `CausalQueries`:

```
install.packages("CausalQueries")
```

To install the latest development release :

```
install.packages("remotes")
remotes::install_github("macartan/CausalQueries")
```

## Causal models

Causal models are defined by:

* A **directed acyclic graph** (DAG), which provides the set of variables, a causal ordering between them, and a set of assumptions regarding conditional independence. If there is no arrow from `A` to `B` then a change in `A` never induces a change in `B`. 
* **Functional forms**. Functional forms describe the causal relationships between nodes. You often have to make strong assumptions when you specify a functional form; fortunately however if variables are categorical  we do not need functional forms in the usual sense. The DAG implies a set of "causal types." Units can be classed together as of the same causal type if they respond to the same way to other variables.  For instance, a type might be the set of units for which `X=1` and for which `Y=1` if and only if `X=1`. The set of causal types grows rapidly with the number of nodes and the number of nodes pointing into any given node. In this setting imposing functional forms is the same as placing *restrictions on causal types*: such restrictions reduce  complexity but require substantive assumptions. An example of a restriction might be "`Y` is monotonic in `X`."
* **Priors.** In the standard case, the DAG plus any restrictions imply a set of parameters that combine to form causal types. These are the  parameters we want to learn about. To learn about them we first provide priors over the parameters. With priors specified the causal model is complete (it is a "probabilistic causal model") and we are ready for inference.  

A wrinkle:

* It is possible that nodes are related in ways not captured by the DAG. In such cases dotted curves are sometimes placed between nodes on a graph. It is possible to specify such possible **unobservable confounding** in the causal model. This has implications for the parameter space.

## Inference

Our goal is to form beliefs over parameters but also over more substantive estimands:

* With a causal model in hand and data available about some or all of the nodes, it is possible to make use of a generic `stan` model that generates posteriors over the parameter vector. 

*  Given updated (or prior) beliefs about parameters it is possible to calculate causal estimands of inference from a causal model. For example "What is the probability that `X` was the cause of `Y` given `X=1`, `Y=1` and `Z=1`."


## Credits etc

The approach used in `CausalQueries` is a generalization of the `biqq` models described in "Mixing Methods: A Bayesian Approach" (Humphreys and Jacobs, 2015,  https://doi.org/10.1017/S0003055415000453). The conceptual extension makes use of work on probabilistic causal models described in Pearl's *Causality* (Pearl, 2009,  https://doi.org/10.1017/CBO9780511803161). The approach to generating a generic `stan` function that can take data from arbitrary models was developed in key contributions by Jasper Cooper (http://jasper-cooper.com/) and Georgiy Syunyaev (http://gsyunyaev.com/).  Lily Medina (https://lilymedina.github.io/) did the magical work of pulling it all together and developing approaches to characterizing confounding and defining estimands. Julio Solis has done wonders to simplify the specification of priors.    
