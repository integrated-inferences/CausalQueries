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
prep_stan_data <- function(model, data, keep_transformed = TRUE, censored_types = NULL) {
    i <- NULL
    # check data is in correct compact form
    if (!all(c("event", "strategy", "count") %in% names(data)))
        stop("Data should contain columns `event`, `strategy` and `count`")

    # 1 Parameter set handlers
    param_set  <- model$parameters_df$param_set
    param_sets <- unique(param_set)
    n_param_sets <- length(param_sets)
    n_param_each <- sapply(param_sets, function(j) sum(param_set == j))
    l_ends <- as.array(cumsum(n_param_each))
    ifelse(length(l_ends) == 1,
           l_starts <- 1,
           l_starts <- c(1, l_ends[1:(n_param_sets - 1)] + 1))
    names(l_starts) <- names(l_ends)

    # 2 Node set handlers
    nodes_sets <- model$parameters_df %>%
      dplyr::mutate(i = 1:n()) %>%
      dplyr::group_by(node) %>%
      dplyr::summarize(n_starts = i[1], n_ends = i[n()], n_node_each = n()) %>%
      dplyr::arrange(n_starts)
    n_starts <- nodes_sets$n_starts
    n_ends <- nodes_sets$n_ends
    n_sets <- nodes_sets$node
    names(n_starts) <- names(n_ends) <- n_sets


    # 2 Data parsing: allowing for censored types
    data <- data[!c(data$event %in% censored_types), ]

    E <- data.frame((get_data_families(model, mapping_only = TRUE)))[data$event,] |>
      as.matrix()

    strategies <- data$strategy
    n_strategies <- length(unique(strategies))
    w_starts <- which(!duplicated(strategies))
    k <- length(strategies)
    w_ends <- {if (n_strategies < 2)
        k
    else
        c(w_starts[2:n_strategies] - 1, k)
    }

    P <- get_parameter_matrix(model)

    # Parmap: mapping from parameters to full data types
    # goes to 0 for data types that never get to be observed
    parmap <- get_parmap(model)

    map <- attr(parmap, "map")
    n_paths <- nrow(map) # The ways params can produce a data type

    # if(!is.null(censored_types))
    #   parmap[, censored_types] <- 0

    list(
        parmap = parmap,
        map = map,
        n_paths = n_paths,
        n_params = nrow(parmap),
        n_param_sets = n_param_sets,
        n_param_each = as.array(n_param_each),
        l_starts = as.array(l_starts),
        l_ends = as.array(l_ends),
        node_starts = as.array(n_starts),
        node_ends = as.array(n_ends),
        n_nodes = length(n_sets),
        lambdas_prior = get_priors(model),
        n_data = all_data_types(model, possible_data = TRUE) |> nrow(),
        n_events = nrow(E),
        n_strategies = n_strategies,
        strategy_starts = as.array(w_starts),
        strategy_ends = as.array(w_ends),
        keep_transformed = as.numeric(keep_transformed),
        E = E,
        Y = as.array(data$count),
        P = P,
        n_types = ncol(P)
    )

}
