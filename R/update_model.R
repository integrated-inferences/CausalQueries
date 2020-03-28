#' Fit causal model using stan
#'
#' Takes a model and data and returns a model object with data attached and a posterior model
#'
#' @inheritParams CausalQueries_internal_inherit_params
#'
#' @param data_type Either 'long' (as made by `simulate_data()`) or 'compact' (as made by \code{\link{collapse_data}}).
#'  Compact data must have entries for each member of each strategy family to produce a valid simplex.
#' @param keep_fit Logical. Whether to append the stanfit object to the model. Defaults to `FALSE`
#' @param ... Options passed onto \code{rstan::stan} call.
#' @import methods
#' @import Rcpp
#' @importFrom rstan stan
#' @importFrom rstan extract
#' @importFrom rstan sampling
#' @export
#' @examples
#' model <- make_model('X->Y')
#' data_long   <- simulate_data(model, n = 4)
#' data_short  <- collapse_data(data_long, model)
#' model_1 <- update_model(model, data_long)
#'
#' # Throws error unless compact data indicated:
#' \dontrun{
#' model_3 <- update_model(model, data_short)
#' }
#' model_4 <- update_model(model, data_short, data_type = 'compact')
#'
#' # It is possible to implement updating without data, in which case the posterior
#' # is a stan object that reflects the prior
#' model5 <- update_model(model)
#'
#' # Advanced: Example of a model with tailored parameters.
#' # We take a model and add a tailored P matrix (which maps from parameters
#' # to causal types) and a tailored parameters_df which reports that
#' # all parameters are in one family.
#' # Parameters in this example are not connected with nodal types in any way.
#'
#' \dontrun{
#' model <- make_model('X->Y')
#' model$P <- diag(8)
#' colnames(model$P) <- rownames(model$causal_types)
#' model$parameters_df <- data.frame(
#'   param_names = paste0('x',1:8),
#'   param_set = 1, priors = 1, parameters = 1/8)
#'
#' # Update fully confounded model on strongly correlated data
#'
#' data <- make_data(make_model('X->Y'), n = 100,
#'   parameters = c(.5, .5, .1,.1,.7,.1))
#' fully_confounded <- update_model(model, data, keep_fit = TRUE)
#' fully_confounded$stan_fit
#' query_model(fully_confounded, 'Y[X = 1] > Y[X=0]', using = 'posteriors')
#' # To see the confounding:
#' with(fully_confounded$posterior_distribution %>% data.frame(),
#' {par(mfrow = c(1,2))
#'  plot(x1, x5, main = 'joint distribution of X0.Y00, X0.Y01')
#'  plot(x1, x6, main = 'joint distribution of X0.Y00, X1.Y01')})
#' }
#'
update_model <- function(model, data = NULL, data_type = "long", keep_fit = FALSE, ...) {

    if (data_type == "long") {

        if (is.null(data)) {

            message("No data provided")
            data_events <- minimal_event_data(model)

        } else {

            if (nrow(data) == 0 | all(is.na(data))) {

                message("No data provided")
                data_events <- minimal_event_data(model)

            } else {

                if (!any(model$nodes %in% names(data)))
                  stop("Data should contain columns corresponding to model nodes")

                data_events <- collapse_data(data, model)
            }

        }
    }

    if (data_type == "compact") {
        if (!all(c("event", "strategy", "count") %in% names(data)))
            stop("Compact data should contain columnes `event`, `strategy` and `count`")
        data_events <- data
    }

    stan_data <- prep_stan_data(model = model, data = data_events)

    # assign fit
    stanfit <- stanmodels$simplexes
    sampling_args <- set_sampling_args(object = stanfit, user_dots = list(...), data = stan_data)
    newfit <- do.call(rstan::sampling, sampling_args)

    if (keep_fit)
        model$stan_fit <- newfit

    posterior_distribution <- extract(newfit, pars = "lambdas")$lambdas
    colnames(posterior_distribution) <- get_parameter_names(model)

    model$posterior_distribution <- posterior_distribution
    model$data <- data

    model
}

