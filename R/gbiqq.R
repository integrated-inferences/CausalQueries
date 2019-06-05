
#' Fit stan model
#'
#' Takes a model and data and returns a model object with data attached and a posterior model
#' @param model A model as created by \code{make_model}
#' @param data A data frame with observations
#' @param ... Arguments passed to \code{rstan::sampling} (e.g. iter, chains).
#' @importFrom rstan sampling
#' @import rstantools
#' @importFrom Rcpp cpp_object_initializer
#' @export
#'
#'
# gbiqq <- function(model, data, ...) {
# 	stan_data    <- make_gbiqq_data(model = model, data = data)
# 	# stan_file <- system.file("src/stan_files" ,"simplexes.stan", package = "gbiqq")
# 	model$posterior_distribution <-	rstan::stan(stanmodels$simplexes, data = stan_data, ...)
# 	# model$posterior_distribution <-	rstan::stan(file = stan_file, data = stan_data, ...)
# 	model$data <- data
# 	model
# 	}

gbiqq <- function(model, data, ...){
	stan_data <- make_gbiqq_data(model = model, data = data)
	model$posterior_distribution <- rstan::sampling(object =
		stanmodels$simplexes, data = stan_data, ...)
	model$data <- data
	model
}
