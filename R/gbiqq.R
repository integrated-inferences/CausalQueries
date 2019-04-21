
#' Fit stan model
#'
#' @param dag A dag as created by \code{make_dag}
#' @param data A data frame with observations
#' @param lambdas_prior A vector containg priors for lambda
#' @importFrom rstan stan
#' @importFrom Rcpp cpp_object_initializer
#' @export
#'
gbiqq <- function(model, data,  P = NULL, ...) {


stan_file <- system.file("tools" ,"simplexes.stan", package = "gbiqq")

stan_data    <- make_gbiqq_data(model = model, data = data, P = P)

fitted_model <-	stan(file = stan_file, data = stan_data,  ...)

return(fitted_model)

}
