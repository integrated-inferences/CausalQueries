#' Fit stan model
#'
#' Takes a model and data and returns a model object with data attached and a posterior model
#' @param model A causal model as created by \code{make_model}
#' @param data A data frame with observations
#' @param stan_model A fitted stan model: if not provided a gbiqq model is compiled from stan file "simplexes.stan"
#' @importFrom rstan stan
#' @importFrom Rcpp cpp_object_initializer
#' @export
#'
#'
gbiqq <- function(model, data, stan_model = NULL,  ...) {

	stan_file <- system.file("tools" ,"simplexes.stan", package = "gbiqq")

	stan_data    <- make_gbiqq_data(model = model, data = data)

	if(is.null(stan_model)) {
		model$posterior_distribution <-	rstan::stan(file = stan_file, data = stan_data,  ...)
	} else {
		model$posterior_distribution <-	rstan::stan(fit = stan_model, data = stan_data,  ...)
	}

	model$data <- data

	model
}


#' Make fitted stan model
#'
#' generates a fitted model
#' @export
#' @examples
#' fit <- fitted_model()
#'
fitted_model <- function() {
	model <- make_model("X->Y")
	gbiqq(model, simulate_data(model, n = 1), refresh = 0)$posterior_distribution
}
