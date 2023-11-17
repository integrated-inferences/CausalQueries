#' Development and Democratization: Data for replication of analysis in
#' *Integrated Inferences*
#'
#' A dataset containing information on inequality, democracy, mobilization,
#' and international pressure.
#' Made by \code{devtools::use_data(democracy_data, CausalQueries)}
#'
#' @format A data frame with 84 rows and 5 nodes:
#' \describe{
#'   \item{Case}{Case}
#'   \item{D}{Democracy}
#'   \item{I}{Inequality}
#'   \item{P}{International Pressure}
#'   \item{M}{Mobilization}
#' }
#' @source \url{https://www.cambridge.org/core/journals/american-political-science-review/article/inequality-and-regime-change-democratic-transitions-and-the-stability-of-democratic-rule/C39AAF4CF274445555FF41F7CC896AE3#fndtn-supplementary-materials/}
"democracy_data"



#' Lipids: Data for Chickering and Pearl replication
#'
#' A compact dataset containing information on an encouragement,
#' (Z, cholestyramine prescription), a treatment (X, usage), and
#' an outcome (Y, cholesterol).
#' From David Maxwell Chickering and Judea Pearl: "A Clinician’s Tool for
#' Analyzing Non-compliance", AAAI-96 Proceedings. Chickering and Pearl in turn
#' draw the data from Efron, Bradley, and David Feldman.
#' "Compliance as an explanatory variable in clinical trials."
#' Journal of the American Statistical Association 86.413 (1991): 9-17.
#'
#' @format A data frame with 8 rows and 3 columns:
#' \describe{
#'   \item{event}{The data type}
#'   \item{strategy}{For which nodes is data available}
#'   \item{count}{Number of units with this data type}
#' }
#' @source \url{https://cdn.aaai.org/AAAI/1996/AAAI96-188.pdf}
"lipids_data"


#' Institutions and growth: Data for replication of analysis in
#' *Integrated Inferences*
#'
#' A  dataset containing dichotomized versions of variables in
#' Rodrik, Subramanian, and Trebbi (2004).
#'
#' @format A data frame with 79 rows and 5 columns:
#' \describe{
#'   \item{Y}{Income (GDP PPP 1995), dichotomized}
#'   \item{R}{Institutions, (based on  Kaufmann, Kraay, and Zoido-Lobaton (2002)) dichotomized}
#'   \item{D}{Distance from the equator (in degrees), dichotomized}
#'   \item{M}{Settler mortality (from Acemoglu, Johnson, and Robinson), dichotomized}
#'   \item{country}{Country}
#' }
#' @source \url{https://drodrik.scholar.harvard.edu/publications/institutions-rule-primacy-institutions-over-geography-and-integration}
"institutions_data"
