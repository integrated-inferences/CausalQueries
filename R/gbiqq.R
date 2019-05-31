
#' Fit stan model
#'
#' Takes a model and data and returns a model object with data attached and a posterior model
#' @param model A model as created by \code{make_model}
#' @param data A data frame with observations
#' @importFrom rstan stan sampling
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
		stanmodels$simplexes, data = stan_data,
		pars = NA, chains = 4, iter = 2000,
		warmup = floor(iter/2), thin = 1,
		init = "random", seed = sample.int(.Machine$integer.max, 1),
		algorithm = c("NUTS", "HMC", "Fixed_param"),
		control = NULL, check_data = TRUE,
		sample_file = NULL,
		diagnostic_file = NA, verbose = FALSE,
		algorithm = match.arg(algorithm), control = NULL,
		check_unknown_args = FALSE, cores = getOption("mc.cores", 1L),
		open_progress = interactive() && !isatty(stdout()) &&
			!identical(Sys.getenv("RSTUDIO"), "1"),
		include = TRUE)
	model$data <- data
	model
}
