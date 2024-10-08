#' Fit causal model using 'stan'
#'
#' Takes a model and data and returns a model object with data
#' attached and a posterior model
#'
#' @inheritParams CausalQueries_internal_inherit_params
#'
#' @param data_type Either 'long' (as made by \code{\link{make_data}}) or
#'   'compact' (as made by \code{\link{collapse_data}}). Compact data must
#'   have entries for each member of each strategy family to produce a
#'   valid simplex. When long form data is provided with missingness, missing
#'   data is assumed to be missing at random.
#' @param keep_type_distribution Logical. Whether to keep the (transformed) distribution
#'   of the causal types.  Defaults to `TRUE`
#' @param keep_event_probabilities Logical. Whether to keep the (transformed) distribution
#'   of event probabilities. Defaults to `FALSE`
#' @param keep_fit Logical. Whether to keep the \code{stanfit} object produced
#'   by \link[rstan]{sampling} for further inspection.
#'   See \code{?stanfit} for more details. Defaults to `FALSE`. Note the  \code{stanfit}
#'   object has internal names for parameters (lambda), event probabilities (w), and the
#'   type distribution (types)
#' @param censored_types vector of data types that are selected out of
#'   the data, e.g. \code{c("X0Y0")}
#' @param ... Options passed onto \link[rstan]{sampling} call. For
#'   details see \code{?rstan::sampling}
#'
#' @return An object of class \code{causal_model}. The returned model is a
#'   list containing the elements comprising a model
#'   (e.g. 'statement', 'nodal_types' and 'DAG') with the
#'   \code{posterior_distribution} returned by \link[rstan]{stan}
#'   attached to it.
#'
#' @seealso \code{\link{make_model}} allows to create new model,
#'   \code{\link{summary.causal_model}} provides summary method for
#'   output objects of class \code{causal_model}
#'
#' @examples
#'  model <- make_model('X->Y')
#'  data_long   <- make_data(model, n = 4)
#'  data_short  <- collapse_data(data_long, model)
#'  model <-  update_model(model, data_long)
#'  model <-  update_model(model, data_short)
#'
#'    # It is possible to implement updating without data, in which
#'    # case the posterior is a stan object that reflects the prior
#'
#'    update_model(model)
#'
#'  \dontrun{
#'
#'    # Censored data types illustrations
#'    # Here we update less than we might because we are aware of filtered data
#'
#'    data <- data.frame(X=rep(0:1, 10), Y=rep(0:1,10))
#'    uncensored <-
#'      make_model("X->Y") |>
#'      update_model(data) |>
#'      query_model(te("X", "Y"), using = "posteriors")
#'
#'    censored <-
#'      make_model("X->Y") |>
#'      update_model(
#'        data,
#'        censored_types = c("X1Y0")) |>
#'      query_model(te("X", "Y"), using = "posteriors")
#'
#'
#'    # Censored data: We learn nothing because the data
#'    # we see is the only data we could ever see
#'    make_model("X->Y") |>
#'      update_model(
#'        data,
#'        censored_types = c("X1Y0", "X0Y0", "X0Y1")) |>
#'      query_model(te("X", "Y"), using = "posteriors")
#'  }
#'
#' @import methods
#' @import Rcpp
#' @import rstantools
#' @importFrom rstan stan
#' @importFrom rstan extract
#' @importFrom rstan sampling
#'
#' @export
update_model <- function(model,
                         data = NULL,
                         data_type = NULL,
                         keep_type_distribution = TRUE,
                         keep_event_probabilities = FALSE,
                         keep_fit = FALSE,
                         censored_types = NULL, ...) {

  # Guess data_type
  if (is.null(data_type)) {
    data_type <- ifelse(all(c("event", "strategy", "count") %in% names(data)),
                        "compact", "long")
  }

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
    if (!all(c("event", "strategy", "count") %in% names(data))) {
      stop(paste(
        "Compact data should contain columns",
        "`event`, `strategy` and `count`"
      ))
    }

    if(!is.integer(data$count)){
      data$count <- as.integer(data$count)
      warning("count column should be integer valued; value has been forced to integer")
    }

    data_events <- data
  }

  stan_data <- prep_stan_data(model = model,
                              data = data_events,
                              keep_type_distribution = keep_type_distribution,
                              censored_types = censored_types)
  # assign fit
  stanfit <- stanmodels$simplexes

  # parameters to drop
  drop_pars <- c("parlam", "parlam2", "gamma", "sum_gammas", "w_full", "w_0")

  if (!keep_event_probabilities) {
    drop_pars <- c(drop_pars, "w")
  }

  if (!keep_type_distribution) {
    drop_pars <- c(drop_pars, "types")
  }


  sampling_args <- set_sampling_args(object = stanfit,
                                     user_dots = list(...),
                                     data = stan_data,
                                     pars = drop_pars,
                                     include = FALSE)

  newfit <- do.call(rstan::sampling, sampling_args)

  model$stan_objects  <- list(data = data)


  # Keep full fit object
  if (keep_fit) {
    model$stan_objects$stanfit <- newfit
  }

  # Retain posterior distribution
  model$posterior_distribution <-
    extract(newfit, pars = "lambdas")$lambdas |>
    as.data.frame()
  colnames(model$posterior_distribution) <- get_parameter_names(model)
  class(model$posterior_distribution) <- "data.frame"

  # Retain type distribution
  if(keep_type_distribution) {
    model$stan_objects$type_distribution <- extract(newfit, pars = "types")$types

    colnames(model$stan_objects$type_distribution) <- colnames(stan_data$P)
    class(model$stan_objects$type_distribution) <- c("matrix", "array")
  }

  # Retain event (pre-censoring) probabilities
  if (keep_event_probabilities) {
    model$stan_objects$event_probabilities <- extract(newfit, pars = "w")$w
    colnames(model$stan_objects$event_probabilities) <- colnames(stan_data$E)
    class(model$stan_objects$event_probabilities) <- c("matrix", "array")
  }

  # Retain stanfit summary with readable names
  # Identify saved parameters
  params <- colnames(model$posterior_distribution)
  if(keep_event_probabilities)
    params <- c(params, colnames(model$stan_objects$event_probabilities))
  if(keep_type_distribution)
    params <- c(params, colnames(model$stan_objects$type_distribution))
  params <- c(params,  "lp__")

  params_labels <- newfit@sim$fnames_oi

  raname_list <-
    lapply(
      X = list(params, params_labels),
      FUN = function(x) vapply(
        X = x,
        FUN = function(y) {
          paste0(y, paste0(
            rep(" ",
                times =
                  max(vapply(c(params, params_labels), nchar, numeric(1))) -
                  nchar(y)),
            collapse = ""))
        },
        FUN.VALUE = character(1),
        USE.NAMES = FALSE)
    )


  model$stan_objects$stan_summary <- utils::capture.output(print(newfit))

  for (i in seq_along(params)) {
    model$stan_objects$stan_summary <-
      gsub(pattern = raname_list[[2]][i],
           replacement = raname_list[[1]][i],
           x = model$stan_objects$stan_summary,
           fixed = TRUE)
  }

  class(model$stan_objects) <- c("stan_objects", "list")

  return(model)
}


