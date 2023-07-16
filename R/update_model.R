#' Fit causal model using 'stan'
#'
#' Takes a model and data and returns a model object with data attached and a posterior model
#'
#' @inheritParams CausalQueries_internal_inherit_params
#'
#' @param data_type Either 'long' (as made by  \code{\link{simulate_data}}) or 'compact' (as made by \code{\link{collapse_data}}).
#'  Compact data must have entries for each member of each strategy family to produce a valid simplex.
#' @param keep_fit Logical. Whether to append the  \code{\link[rstan]{stanfit}} object to the model. Defaults to `FALSE`
#' @param keep_transformed Logical. Whether to keep transformed parameters, prob_of_types, P_lambdas, w, w_full
#' @param censored_types vector of data types that are selected out of the data, eg c("X0Y0")
#' @param ... Options passed onto \code{\link[rstan]{stan}} call.
#' @return An object of class \code{causal_model}. It essentially returns a list containing the elements comprising
#' a model (e.g. 'statement', 'nodal_types' and 'DAG') with the `posterior_distribution` returned by \code{\link[rstan]{stan}} attached to it.
#' @import methods
#' @import Rcpp
#' @import rstantools
#' @importFrom rstan stan
#' @importFrom rstan extract
#' @importFrom rstan sampling
#' @export
#' @examples
#'
#' model <- make_model('X->Y')
#' data_long   <- simulate_data(model, n = 4)
#' data_short  <- collapse_data(data_long, model)
#'\donttest{
#' model_1 <- update_model(model, data_long)
#'}
#'\donttest{
#' model_2 <- update_model(model, data_long, keep_transformed = FALSE)
#'}
#'\dontrun{
#' # Throws error unless compact data indicated:
#'
#' model_3 <- update_model(model, data_short)
#'}
#'\donttest{
#' model_4 <- update_model(model, data_short, data_type = 'compact')
#'
#' # It is possible to implement updating without data, in which case the posterior
#' # is a stan object that reflects the prior
#' model_5 <- update_model(model)
#'
#'
#' # Censored data types
#' make_model("X->Y") %>%
#'   update_model(data.frame(X=c(1,1), Y=c(1,1)), censored_types = c("X1Y0")) %>%
#'   query_model(te("X", "Y"), using = "posteriors")
#'
#'# Censored data: Learning nothing
#' make_model("X->Y") %>%
#'   update_model(data.frame(X=c(1,1), Y=c(1,1)), censored_types = c("X1Y0", "X0Y0", "X0Y1")) %>%
#'   query_model(te("X", "Y"), using = "posteriors")
#'   }

update_model <- function(model, data = NULL, data_type = "long", keep_fit = FALSE,
                         keep_transformed = TRUE, censored_types = NULL, ...) {

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

    stan_data <- prep_stan_data(model = model,
                                data = data_events,
                                keep_transformed = keep_transformed*1)

    if(stan_data$n_nodes == 1){
      stan_data$n_param_each <- as.array(stan_data$n_param_each)
      stan_data$l_starts <- as.array(stan_data$l_starts)
      stan_data$l_ends <- as.array(stan_data$l_ends)
      stan_data$node_starts <- as.array(stan_data$node_starts)
      stan_data$node_ends <- as.array(stan_data$node_ends)
    }

    # Parmap goes to 0 for data types that never get to be observed
    if(!is.null(censored_types))
    stan_data$parmap[, censored_types] <- 0

    # assign fit
    stanfit <- stanmodels$simplexes

    sampling_args <- set_sampling_args(object = stanfit, user_dots = list(...), data = stan_data)
    newfit <- do.call(rstan::sampling, sampling_args)

    model$stan_objects  <- list(data = data)

    if(keep_fit) model$stan_objects$stan_fit <- newfit

    model$posterior_distribution <- extract(newfit, pars = "lambdas")$lambdas
        colnames(model$posterior_distribution) <- get_parameter_names(model)

    model$stan_objects$type_distribution <- extract(newfit, pars = "prob_of_types")$prob_of_types
        colnames(model$stan_objects$type_distribution) <- colnames(stan_data$P)

    model$stan_objects$w_full <- extract(newfit, pars = "w_full")$w_full
        colnames(model$stan_objects$w_full) <- rownames(stan_data$E)

    model

}


