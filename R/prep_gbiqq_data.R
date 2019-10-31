#' Prepare data for stan
#'
#' Create a list containing the data to be passed to stan
#'
#' @param model A model created by \code{make_model}
#' @param data A  "compact" data frame (as made by `simulate_events()``)
#' @return a list
#' @export
#' @examples
#' model <- make_model("X->Y")
#' data  <-  collapse_data(simulate_data(model, n = 6), model)
#' prep_gbiqq_data(model, data)
#'
#' model <- make_model("X->Y") %>% set_confound(list(X = "Y[X=1]>Y[X=0]"))
#' data  <-  collapse_data(simulate_data(model, n = 6), model)
#' prep_gbiqq_data(model, data)
#'
prep_gbiqq_data <- function(model, data){

	if(!all(c("event", "strategy", "count") %in% names(data))) stop("Data should contain columns `event`, `strategy` and `count`")

	P                  <- get_parameter_matrix(model)
	param_set          <- model$parameters_df$param_set
	model$priors       <- get_priors(model)
	param_sets         <- unique(param_set)
	n_param_sets       <- length(param_sets)
	inverted_P         <- 1-P
#	A_w                <- (get_data_families(model)$A_w)[data$event, ]
	A_w                <- (get_data_families(model, mapping_only = TRUE))[data$event, ]
	strategies         <- data$strategy
	n_strategies       <- length(unique(strategies))
	w_starts           <- which(!duplicated(strategies))
	k                  <- length(strategies)
	w_ends             <- if(n_strategies < 2) k else c(w_starts[2:n_strategies]-1, k)
	n_param_each       <- sapply(param_sets, function(j) sum(param_set ==j))
	l_ends             <- cumsum(n_param_each)
	l_starts           <- c(1, l_ends[1:(n_param_sets-1)] + 1)
	names(l_starts) <- names(l_ends)

 list(n_params        = nrow(P),
			n_param_sets    = n_param_sets,
			n_param_each    = n_param_each,
			l_starts        = l_starts,
			l_ends          = l_ends,
			lambdas_prior   = model$priors,
			n_types         = ncol(P),
			n_data          = nrow(get_max_possible_data(model)),
			n_events        = nrow(A_w),
			n_strategies    = n_strategies,
			strategy_starts = as.array(w_starts),
			strategy_ends   = as.array(w_ends),
			P               = P,
			inverted_P      = inverted_P,
			A               = get_ambiguities_matrix(model),
			A_w             = A_w,
			Y               = data$count)
}

