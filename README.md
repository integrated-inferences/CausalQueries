# gbiqq

`gbiqq` is a package that lets you declare binary causal models, update beliefs about causal types given data and calculate arbitrary estimands. Model definition makes use of `dagitty` functionality. Updating is implemented in `stan`. 

See [here](https://macartan.github.io/causalmodels/) for a guide to using `gbiqq` along with many examples of causal models  

## Causal models

Causal models are defined by:

* A **directed acyclic graph** (DAG), which provides the set of (binary) variables, a causal ordering between them, and a set of assumptions regarding conditional independence. If there is no arrow from A to B then a change in A never induces a change in B. 
* **Functional forms**. In this binary world we do not need functional forms in the usual sense. The DAG implies a set of "causal types." Units are classed together as of the same causal type if they respond to the same way to other variables.  For instance, a type might be the set of units for which `X=1` and for which `Y=1` if and only if `X=1`. The set of causal types grows rapidly with the number of nodes and the number of nodes pointing into any given node. In this setting imposing functional forms is the same as placing *restrictions on causal types*: such restrictions reduce  complexity but require substantive assumptions. An example of a restriction might be "`Y` is monotonic in `X`."
* **Priors.** In the standard case, the DAG plus any restrictions imply a set of parameters that combine to form causal types. These are the  parameters we want to learn about. To learn about them we first provide priors over the parameters. With priors specified the causal model is complete (it is a "probabilistic causal model") and we are ready for inference.  

A wrinkle:

* It is possible that nodes are related in ways not captured by the DAG. In such cases dotted curves are sometimes placed between nodes on a graph. It is possible to specify such possible **unobservable confounding** in the causal model. This has implications for the parameter space.

## Inference

Our goal is to form beliefs over parameters but also over more substantive estimands:

* With a causal model in hand and data available about some or all of the nodes, it is possible to make use of a generic `stan` model that generates posteriors over the parameter vector. 

*  Given updated (or prior) beliefs about parameters it is possible to calculate causal estimands of inference from a causal model. For example "What is the probably that `X` was the cause of `Y` given `X=1`, `Y=1` and `Z=1`."

## Example

Here is an example of a model in which `X` causes `M` and  `M` causes `Y`. There is, in addition, unobservable confounding between `X` and `Y`. This is an example of a model in which you might use information on `M` to figure out whether `X` caused `Y`.

The DAG is defined using `dagitty` syntax like this:

```
model <- make_model("X -> M -> Y")
```

To add the confounding we have to allow an additional parameter that allows a possibly different assignment probability for `X` given a causal type for `Y`.


```
model <- set_confound(model, list(X = "Y[X=1] == 1"))
```

We then set priors thus:

```
model <- set_priors(model, distribution = "jeffreys")
```

You can plot the dag, making use of functions in the `dagitty` package. 

```{r}
plot(model)
```

You can draw data from the model, like this:

```
data <- make_data(model, n = 10)
```


Updating is done like this:


```
updated_model <- update_model(model, data)
```

Finally you can calculate an estimand of interest like this:

``` 
CoE <- query_distribution(
                   model = updated_model, 
                   using = "posteriors",
                   query = "Y[X=0] == 0",
                   subset = "X==1 & Y==1"
                   )
```

This uses the posterior distribution and the model to assess the "causes of effects" estimand: the probability that `X=1` was the cause of `Y=1` in those cases in which `X=1` and `Y=1`. The approach is to imagine a set of "do" operations on the model, that control the level of `X` and to inquire about the level of `Y` given these operations, and then to assess how likely is is that `Y` would be 0 if `X` were fixed at 0 within a set that naturally take on particular values of `X` and `Y`. By the same token this posterior can be calculated conditional on observations of `M`, allowing an assessment of how data on mediators alters inference about the causes of effects.

## Credits etc

The approach used in `gbiqq` is a generalization of the `biqq` models described in "Mixing Methods: A Bayesian Approach" (Humphreys and Jacobs, 2015,  https://doi.org/10.1017/S0003055415000453). The conceptual extension makes use of work on probabilistic causal models described in Pearl's *Causality* (Pearl, 2009,  https://doi.org/10.1017/CBO9780511803161). The approach to generating a generic `stan` function that can take data from arbitrary models was developed in key contributions by Jasper Cooper (http://jasper-cooper.com/) and Georgiy Syunyaev (http://gsyunyaev.com/).  Lily Medina (https://lilymedina.github.io/) did the magical work of pulling it all together and developing approaches to characterizing confounding and defining estimands. Julio Solis has done wonders to simplify the specification of priors.    
