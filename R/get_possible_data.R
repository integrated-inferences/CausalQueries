

#' Get Possible Data
#'
#' Creates a list of all possible data realizations for each node given each possible parent value
#' @param model A probabilistic causal model created by \code{make_model()}
#' @param collapse whether to collapse possible data
#'
#' @export
get_possible_data <- function(model, collapse = TRUE){

	revealed_data <- reveal_outcomes(model)
	variables <- model$variables
	parents <- get_parents(model)

	possible_data <- lapply(variables, function(variable){
		var_parents <- parents[variable][[1]]
		var_variables <- c(var_parents,variable)
		out <- revealed_data[,var_variables]
		duplicates <- duplicated(out)
		if(length(	var_variables) == 1) {
			out <- matrix(as.double(out[!duplicates]), ncol = length(var_variables))
		} else {
			out <- out[!duplicates, ]
		}
		colnames(out) <- var_variables
		out
	})
	names(possible_data) <- variables
	if(collapse){
		possible_data <- lapply(variables, function(variable){
			mat <- possible_data[[variable]]
			apply(mat, 1, function(x) paste0(variable, x, collapse = ""))
		})
	}

	names(possible_data) <- variables
	possible_data
}

#' Get data frame with all possible data combinations
#'
#' @param model A model created by make_model()
#' @export
#' @return A data frame
#'
get_max_possible_data <- function(model) {

	max_possible_data <-
		Reduce(f = merge,
					 x = get_possible_data(model, collapse = FALSE))
	variables <- 	model$variables
	max_possible_data <- max_possible_data[variables]

	max_possible_data <-
		max_possible_data[do.call("order", as.list(max_possible_data[,rev(names(max_possible_data))])),]

	rownames(max_possible_data) <- 1:nrow(max_possible_data)

	return(max_possible_data)
}
