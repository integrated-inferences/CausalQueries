#' Draw a single causal type given a parameter vector
#'
#' Output is a parameter data frame recording both parameters
#' (case level priors) and
#' the case level causal type.
#'
#' @inheritParams CausalQueries_internal_inherit_params
#' @param ... Arguments passed to \code{\link{set_parameters}}
#' @export
#' @examples
#'
#' # Simple draw using model's parameter vector
#' make_model("X -> M -> Y") |>
#' draw_causal_type()
#'
#' # Draw parameters from priors and draw type from parameters
#' make_model("X -> M -> Y") |>
#' draw_causal_type(, param_type = "prior_draw")
#'
#' # Draw type given specified parameters
#' make_model("X -> M -> Y") |>
#' draw_causal_type(parameters = 1:10)
#'


draw_causal_type <- function(model, ...){
  param_value <- NULL
  set_parameters(model, ...)$parameters_df |>
    group_by(param_set) |>
    mutate(causal_type = rmultinom(1, 1, param_value)[,1]) |>
    ungroup()
}
