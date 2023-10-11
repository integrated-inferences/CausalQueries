#' Fit causal model using 'stan'
#'
#' Takes a model and data and returns a model object with data attached and a posterior model
#'
#' @inheritParams CausalQueries_internal_inherit_params
#'
#' @param data_type Either 'long' (as made by  \code{\link{make_data}}) or 'compact' (as made by \code{\link{collapse_data}}).
#'  Compact data must have entries for each member of each strategy family to produce a valid simplex.
#' @param keep_fit Logical. Whether to append the  \code{\link[rstan]{stanfit}} object to the model. Defaults to `FALSE`
#' @param keep_w Logical. Whether to keep the distribution of event probabilities. Defaults to `FALSE`
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
#' model_4 <- update_model(model, data_short, data_type = 'compact')
#'
#' # It is possible to implement updating without data, in which
#' # case the posterior is a stan object that reflects the prior
#' model_5 <- update_model(model)
#'
#'
#' # Censored data types
#' # We update less than we might because we are aware of filtered data
#' uncensored <-  make_model("X->Y") %>%
#'   update_model(data.frame(X=rep(0:1, 10), Y=rep(0:1,10))) |>
#'   query_model(te("X", "Y"), using = "posteriors")
#'
#' censored <- make_model("X->Y") %>%
#'   update_model(data.frame(X=rep(0:1, 10), Y=rep(0:1,10)),
#'   censored_types = c("X1Y0")) %>%
#'   query_model(te("X", "Y"), using = "posteriors")
#'
#' # Censored data: We learning nothing because the data
#' # we see is the only data we could ever see
#' make_model("X->Y") %>%
#'   update_model(data.frame(X=rep(1,5), Y=rep(1,5)),
#'   censored_types = c("X1Y0", "X0Y0", "X0Y1")) %>%
#'   query_model(te("X", "Y"), using = "posteriors")
#'}


update_model <- function(model, data = NULL, data_type = NULL, keep_fit = FALSE,
                         keep_transformed = TRUE, keep_w = FALSE, censored_types = NULL, ...) {

   # Guess data_type
  if(is.null(data_type))
    data_type <- ifelse(all(c("event", "strategy", "count") %in% names(data)), "compact", "long")

  # Checks on data_types
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
                                keep_transformed = keep_transformed,
                                censored_types = censored_types)
    # assign fit
    stanfit <- stanmodels$simplexes

    sampling_args <-
      set_sampling_args(object = stanfit, user_dots = list(...), data = stan_data)

    newfit <- do.call(rstan::sampling, sampling_args)

    model$stan_objects  <- list(data = data)

    if(keep_fit) model$stan_objects$stan_fit <- newfit

    # Retain posterior distribution
    model$posterior_distribution <-
      extract(newfit, pars = "lambdas")$lambdas |> as.data.frame()
    colnames(model$posterior_distribution) <- get_parameter_names(model)

    # Retain type distribution
    model$stan_objects$type_distribution <-
      extract(newfit, pars = "prob_of_types")$prob_of_types

    colnames(model$stan_objects$type_distribution) <- colnames(stan_data$P)

    # Retain event (pre-censoring) probabilities
    if(keep_w){
    model$stan_objects$w <- extract(newfit, pars = "w")$w
        colnames(model$stan_objects$w) <- colnames(stan_data$E)
    }

    model

}


