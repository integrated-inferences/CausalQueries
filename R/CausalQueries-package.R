#' CausalQueries
#'
#' A package for creating and updating  binary probabilistic causal models
#'
#' @docType package
#'
#' @name CausalQueries-package
#' @importFrom utils globalVariables
#' @useDynLib CausalQueries, .registration = TRUE

globalVariables(names = c("posterior", "prob", "restrict", "n", "model", "data", "%>%", "node", "nodal_type",
    "param_set", "event", "count", "strategy",  "distinct", "e", "v", "w"))
