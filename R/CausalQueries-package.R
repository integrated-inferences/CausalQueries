#' CausalQueries
#'
#' CausalQueries is a package that lets you declare binary causal models,
#' update beliefs about causal types given data and calculate arbitrary estimands.
#' Model definition makes use of dagitty functionality. Updating is implemented in stan.
#' @docType package
#'
#' @name CausalQueries-package
#' @importFrom utils globalVariables
#' @useDynLib CausalQueries, .registration = TRUE

globalVariables(names = c("posterior", "prob", "restrict", "n", "model", "data", "%>%", "node", "nodal_type",
    "param_set", "event", "count", "strategy",  "distinct", "e", "v", "w"))
