#' Prepare data for stan
#'
#' Create a list containing the data to be passed to stan
#'
#' @inheritParams CausalQueries_internal_inherit_params
#' @return A \code{list}.
#' @keywords internal
#' @examples
#' model <- make_model('X->Y')
#' data  <-  collapse_data(simulate_data(model, n = 6), model)
#' CausalQueries:::prep_stan_data(model, data)
#'
#' model <- make_model('X->Y') %>% set_confound(list(X = 'Y[X=1]>Y[X=0]'))
#' data  <-  collapse_data(simulate_data(model, n = 6), model)
#' CausalQueries:::prep_stan_data(model, data)
#'
prep_stan_data <- function(model, data) {

    if (!all(c("event", "strategy", "count") %in% names(data)))
        stop("Data should contain columns `event`, `strategy` and `count`")

    A <- get_ambiguities_matrix(model)
    P <- get_parameter_matrix(model)
    param_set <- model$parameters_df$param_set
    param_sets <- unique(param_set)
    n_param_sets <- length(param_sets)
    E <- (get_data_families(model, mapping_only = TRUE))[data$event, ]
    strategies <- data$strategy
    n_strategies <- length(unique(strategies))
    w_starts <- which(!duplicated(strategies))
    k <- length(strategies)
    w_ends <- if (n_strategies < 2)
        k else c(w_starts[2:n_strategies] - 1, k)
    n_param_each <- sapply(param_sets, function(j) sum(param_set == j))
    l_ends <- as.array(cumsum(n_param_each))
    l_starts <- c(1, l_ends[1:(n_param_sets - 1)] + 1)
    names(l_starts) <- names(l_ends)

    list(n_params = nrow(P), n_param_sets = n_param_sets, n_param_each = as.array(n_param_each), l_starts = as.array(l_starts),
        l_ends = as.array(l_ends), lambdas_prior = get_priors(model), n_types = ncol(P), n_data = nrow(all_data_types(model,
            possible_data = TRUE)), n_events = nrow(E), n_strategies = n_strategies, strategy_starts = as.array(w_starts),
        strategy_ends = as.array(w_ends), P = P, not_P = 1 - P, A = A, E = E, Y = data$count)
}


