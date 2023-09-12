#' Prepare data for 'stan'
#'
#' Create a list containing the data to be passed to 'stan
#'
#' @inheritParams CausalQueries_internal_inherit_params
#' @return A \code{list} containing data to be passed to 'stan'
#' @keywords internal
#' @examples
#' \donttest{
#' model <- make_model('X->Y')
#' data  <-  collapse_data(make_data(model, n = 6), model)
#' CausalQueries:::prep_stan_data(model, data)
#'
#' model <- make_model('X->Y') %>% set_confound(list(X = 'Y[X=1]>Y[X=0]'))
#' data  <-  collapse_data(make_data(model, n = 6), model)
#' CausalQueries:::prep_stan_data(model, data)
#' }
#'
prep_stan_data <- function(model, data, keep_transformed = TRUE) {

    i <- NULL

    if (!all(c("event", "strategy", "count") %in% names(data)))
        stop("Data should contain columns `event`, `strategy` and `count`")
    parmap <- get_parmap(model)

    map <- attr(parmap, "map")
    n_paths <- nrow(map) # The ways params can produce a data type

    # Parameter set handlers
    param_set <- model$parameters_df$param_set
    param_sets <- unique(param_set)
    n_param_sets <- length(param_sets)
    n_param_each <- sapply(param_sets, function(j) sum(param_set == j))
    l_ends <- as.array(cumsum(n_param_each))
    ifelse(length(l_ends) == 1,
           l_starts <- 1,
           l_starts <- c(1, l_ends[1:(n_param_sets - 1)] + 1))
    names(l_starts) <- names(l_ends)

    # Node set handlers
    nodes_sets <- model$parameters_df %>%
      dplyr::mutate(i = 1:n()) %>%
      dplyr::group_by(node) %>%
      dplyr::summarize(n_starts = i[1], n_ends = i[n()], n_node_each = n()) %>%
      dplyr::arrange(n_starts)
    n_starts <- nodes_sets$n_starts
    n_ends <- nodes_sets$n_ends
    n_sets <- nodes_sets$node
    names(n_starts) <- names(n_ends) <- n_sets


    E <-
        (get_data_families(model, mapping_only = TRUE))[data$event,]
    strategies <- data$strategy
    n_strategies <- length(unique(strategies))
    w_starts <- which(!duplicated(strategies))
    k <- length(strategies)
    w_ends <- if (n_strategies < 2)
        k
    else
        c(w_starts[2:n_strategies] - 1, k)
    P <- get_parameter_matrix(model)

    list(
        parmap = parmap,
        map = map,
        n_paths = n_paths,
        n_params = nrow(parmap),
        n_param_sets = n_param_sets,
        n_param_each = as.array(n_param_each),
        l_starts = as.array(l_starts),
        l_ends = as.array(l_ends),
        node_starts = n_starts,
        node_ends = n_ends,
        n_nodes = length(n_sets),
        lambdas_prior = get_priors(model),
        n_data = nrow(all_data_types(model,
                                     possible_data = TRUE)),
        n_events = nrow(E),
        n_strategies = n_strategies,
        strategy_starts = as.array(w_starts),
        strategy_ends = as.array(w_ends),
        keep_transformed = keep_transformed * 1,
        E = E,
        Y = data$count,
        P = P,
        n_types = ncol(P)
    )

}
