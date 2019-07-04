## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
library(knitr)
library(gbiqq)
library(dplyr)

## ---- message = FALSE----------------------------------------------------
model <- 
	make_model("X -> M -> Y") %>%
	set_parameter_matrix(confound = list(c(X="X"), list( Y = "11" )))%>%
	set_priors()

## ------------------------------------------------------------------------
plot_dag(model)

## ------------------------------------------------------------------------
# true parameter vector (lambda)
true_lambda = c(.8, .2, .5, .5, .25, .5, 0, .25, .25, .5, 0, .25)
           

