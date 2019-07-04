## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
knitr::opts_chunk$set(echo = TRUE)
library(knitr)
library(gbiqq)
library(dplyr)

## ------------------------------------------------------------------------
model    <- make_model("X -> Y") %>%
	
	          set_priors(prior_distribution = "jeffreys")

## ------------------------------------------------------------------------
plot_dag(model)

## ------------------------------------------------------------------------
get_parameter_matrix(model)

## ------------------------------------------------------------------------
data <- simulate_data(model, n = 5000, parameters = c(.5, .5, .25, .5, 0, .25))
table(data)

## ---- message = FALSE, warning = FALSE, results = "hide"-----------------
updated_model      <- gbiqq(model, data, refresh = 0)

## ------------------------------------------------------------------------
updated_model$posterior_distribution

