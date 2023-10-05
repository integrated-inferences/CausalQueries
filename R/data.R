#' Democracy Data
#'
#' A dataset containing information on inequality, democracy, mobilization, and international pressure.
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



#' Chickering Pearl Data
#'
#' A compact dataset containing information on an encouragement, (Z, cholestyramine prescription), a treatment (X, usage), and an outcome (Y, cholesterol).
#' From David Maxwell Chickering and Judea Pearl: "A Clinician’s Tool for Analyzing Non-compliance", AAAI-96 Proceedings.
#'
#' @format A data frame with 8 rows and 3 columns:
#' \describe{
#'   \item{event}{The data type}
#'   \item{strategy}{For which nodes is data available}
#'   \item{count}{Number of units with this data type}
#' }
#' @source \url{https://cdn.aaai.org/AAAI/1996/AAAI96-188.pdf}
"chickering_pearl"
