#' gbiqq: A package for creating and updating  binary probabilistic causal models
#'
#' @docType package
#'
#' @name gbiqq-package
#' @importFrom utils globalVariables
#' @useDynLib gbiqq, .registration = TRUE
#' @name gbiqq-package

globalVariables(names = c(
	"posterior",
	"prob",
	"restrict",
	"n",
	"model",
	"data",
	"%>%",
	"node",
	"nodal_type",
	"param_set"
	))
