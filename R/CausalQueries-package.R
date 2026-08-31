#' CausalQueries: Make, Update, and Query Binary Causal Models
#'
#' 'CausalQueries' is a package that lets users generate binary causal models,
#' update over models given data, and calculate arbitrary causal queries.
#' Model definition makes use of dagitty type syntax.
#' Updating is implemented in 'stan'.
#'
#' @references
#' Tietz T, Medina L, Syunyaev G, Humphreys M (2026).
#' "Making, Updating, and Querying Causal Models with CausalQueries."
#' \emph{Journal of Statistical Software}, \bold{117}(1), 1--40.
#' \doi{10.18637/jss.v117.i01}.
#'
#' @importFrom utils globalVariables
#' @useDynLib CausalQueries, .registration = TRUE
"_PACKAGE"

globalVariables(names = c("posterior", "prob", "restrict", "n", "model", "data",
                          "%>%", "node", "nodal_type","param_set", "event",
                          "count", "strategy",  "distinct", "e", "v", "w",
                          "gen","children","given","param_value","priors",
                          "g", "label", "query"
    ))
