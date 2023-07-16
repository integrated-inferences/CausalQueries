#' 'CausalQueries'
#'
#' 'CausalQueries' is a package that lets you generate binary causal models,
#' update over models given data and calculate arbitrary causal queries.
#' Model definition makes use of dagitty syntax. Updating is implemented in 'stan'.
#' @docType package
#'
#' @name CausalQueries-package
#' @importFrom utils globalVariables
#' @useDynLib CausalQueries, .registration = TRUE

globalVariables(names = c("posterior", "prob", "restrict", "n", "model", "data",
                          "%>%", "node", "nodal_type","param_set", "event",
                          "count", "strategy",  "distinct", "e", "v", "w",
                          "gen","children","given","param_value","priors",
                          "g"
    ))
