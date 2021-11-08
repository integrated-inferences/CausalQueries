#' Make data in compact form
#'
#' Draw \code{n} events given event probabilities. Draws full data only. For incomplete data see
#' \code{\link{make_data}}.
#'
#' @inheritParams CausalQueries_internal_inherit_params
#' @param n An integer. Number of observations.
#' @param w A numeric matrix. A `n_parameters x 1` matrix of event probabilities with named rows.
#' @param param_type A character. String specifying type of parameters to make ('flat', 'prior_mean', 'posterior_mean', 'prior_draw', 'posterior_draw', 'define). With param_type set to \code{define} use arguments to be passed to \code{make_priors}; otherwise \code{flat} sets equal probabilities on each nodal type in each parameter set; \code{prior_mean}, \code{prior_draw}, \code{posterior_mean}, \code{posterior_draw} take parameters as the means or as draws from the prior or posterior.
#' @param include_strategy Logical. Whether to include a 'strategy' vector. Defaults to FALSE. Strategy vector does not vary with full data but expected by some functions.
#' @param ... Arguments to be passed to make_priors if param_type == \code{define}
#' @return A \code{data.frame} of events
#' @importFrom stats rmultinom
#' @export
#'
#' @examples
#' \donttest{
#' model <- make_model('X -> Y')
#' make_events(model = model)
#' make_events(model = model, param_type = 'prior_draw')
#' make_events(model = model, include_strategy = TRUE)
#' }
#'
make_events <- function(model, n = 1, w = NULL, P = NULL, A = NULL,
                        parameters = NULL, param_type = NULL,
                        include_strategy = FALSE, ...) {
    # Check whether w is a matrix with named rows
    if (!is.null(w)) {
        if (!is.matrix(w))
            stop("w has to be a matrix.")
        if (length(rownames(w)) != nrow(w))
            stop("w has to be a matrix with named rows.")
    }

    # Check that parameters sum to 1 in each param_set
    if (!is.null(parameters))
        parameters <- clean_param_vector(model, parameters)

    # If parameters not provided, take from model
    if (is.null(parameters)) {
        if (!is.null(param_type)) {
            parameters <- make_parameters(model, param_type = param_type, ...)
        } else {
            parameters <- get_parameters(model)
        }
    }

    if (is.null(w)) {
        if (is.null(P))
            P <- get_parameter_matrix(model)
        if (is.null(A))
            A <- get_ambiguities_matrix(model)
        w <- get_event_prob(model, P, A, parameters = parameters)
    }

    # Draw events (Compact dataframe)
    df <- data.frame(event = rownames(w), count = rmultinom(1, n, w))

    if (include_strategy) {
        df$strategy <- paste0(model$nodes, collapse = "")
        df <- df[, c("event", "strategy", "count")]
    }

    df
}

