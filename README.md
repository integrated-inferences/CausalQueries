# CausalQueries

<!-- badges: start -->
[![CRAN status](https://www.r-pkg.org/badges/version/CausalQueries)](https://CRAN.R-project.org/package=CausalQueries)
![](https://cranlogs.r-pkg.org/badges/grand-total/CausalQueries)
<!-- badges: end -->

`CausalQueries` is a package that lets you declare binary causal models, update beliefs about causal types given data and calculate arbitrary estimands. Model definition makes use of `dagitty` functionality. Updating is implemented in `stan`. 

* See vignettes for a guide to getting started.

* See [here](https://macartan.github.io/assets/pdf/papers/2024_CausalQueries.pdf) for a guide to using `CausalQueries` along with many examples of causal models  

* See [website](https://integrated-inferences.github.io/CausalQueries/) for a comprehensive overview of `CausalQueries`

 

## Installation

To install the latest stable release of `CausalQueries`:

```
install.packages("CausalQueries")
```

To install the latest development release :

```
install.packages("devtools")
devtools::install_github("integrated-inferences/CausalQueries")
```

## Helping Out \& Contributing

`CausalQueries` is an open and active developer community. We welcome contributions and are excited you are keen to get involved.
Please refer to `CONTRIBUTING.md` to get started. 

## Causal Models

Causal models are defined by:

* A **directed acyclic graph** (DAG), which provides the set of variables, a causal ordering between them, and a set of assumptions regarding conditional independence. If there is no arrow from `A` to `B` then a change in `A` never induces a change in `B`. 
* **Functional forms**. Functional forms describe the causal relationships between nodes. You often have to make strong assumptions when you specify a functional form; fortunately however if variables are categorical  we do not need functional forms in the usual sense. The DAG implies a set of "causal types." Units can be classed together as of the same causal type if they respond to the same way to other variables.  For instance, a type might be the set of units for which `X=1` and for which `Y=1` if and only if `X=1`. The set of causal types grows rapidly with the number of nodes and the number of nodes pointing into any given node. In this setting imposing functional forms is the same as placing *restrictions on causal types*: such restrictions reduce  complexity but require substantive assumptions. An example of a restriction might be "`Y` is monotonic in `X`."
* **Priors.** In the standard case, the DAG plus any restrictions imply a set of parameters that combine to form causal types. These are the  parameters we want to learn about. To learn about them we first provide priors over the parameters. With priors specified the causal model is complete (it is a "probabilistic causal model") and we are ready for inference.  Setting priors is done using the `set_priors` function and many examples can be seen by typing `? set_priors.R`.


A wrinkle:

* It is possible that nodes are related in ways not captured by the DAG. In such cases dotted curves are sometimes placed between nodes on a graph. It is possible to specify such possible **unobservable confounding** in the causal model. This has implications for the parameter space.

## Inference

Our goal is to form beliefs over parameters but also over more substantive estimands:

* With a causal model in hand and data available about some or all of the nodes, it is possible to make use of a generic `stan` model that generates posteriors over the parameter vector. 

*  Given updated (or prior) beliefs about parameters it is possible to calculate causal estimands of inference from a causal model. For example "What is the probability that `X` was the cause of `Y` given `X=1`, `Y=1` and `Z=1`."


## Credits etc

The approach used in `CausalQueries` is developed in [Humphreys and Jacobs 2023](https://macartan.github.io/integrated_inferences/) drawing on  work on probabilistic causal models described in Pearl's [*Causality*](https://www.cambridge.org/core/books/causality/B0046844FAE10CBF274D4ACBDAEB5F5B) (Pearl, 2009). We thank Ben Goodrich who provided generous insights on using `stan` for this project. We thank Alan M Jacobs for key work developing the framework underlying the package. Our thanks to Jasper Cooper for contributions generating a generic function to create Stan code, to [Clara Bicalho](https://clarabicalho.github.io/) who helped figure out the syntax for causal statements, to [Julio S. Solís Arce](https://www.gov.harvard.edu/directory/julio-s-solis-arce/) who made many key contributions figuring out how to simplify the specification of priors, and to [Merlin Heidemanns](https://merlinheidemanns.github.io/website/) who figured out the `rstantools` integration and made myriad code improvements. 
