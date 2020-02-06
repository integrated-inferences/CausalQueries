#' gbiqq: A package for creating and updating  binary probabilistic causal models
#'
#' @docType package
#'
#' @name gbiqq-package
#' @importFrom utils globalVariables
#' @useDynLib gbiqq, .registration = TRUE
#' @importFrom rstan sampling
#' @name gbiqq-package
NULL

globalVariables(names = c(
	"posterior",
	"prob",
	"restrict",
	"n",
	"model",
	"data"
	))
