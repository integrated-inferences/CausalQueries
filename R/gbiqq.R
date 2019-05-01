
#' Fit stan model
#'
#' Takes a model and data and returns a model object with data attached and a posterior model
#' @param dag A dag as created by \code{make_dag}
#' @param data A data frame with observations
#' @importFrom rstan stan
#' @importFrom Rcpp cpp_object_initializer
#' @export
#'
#'
gbiqq <- function(model, data,   ...) {

	stan_file <- system.file("tools" ,"simplexes.stan", package = "gbiqq")

	stan_data    <- make_gbiqq_data(model = model, data = data)

	model$posterior <-	rstan::stan(file = stan_file, data = stan_data,  ...)

	model$data <- data

	model

	}
