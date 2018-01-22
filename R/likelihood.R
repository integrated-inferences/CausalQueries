#' get_likelihood_helpers
#'
#' Produce the indices and ambiguity matrix needed for the
#'
#' @param dag A dag created by make_dag()
#'
#' @return Returns indices and ambiguity matrix
#'
#'
#' @export

get_likelihood_helpers <- function(dag){

	# Get variables in order of exogeneity
	variables <- c(
		gbiqq::get_exogenous_vars(test_dag),
		gbiqq::get_endogenous_vars(test_dag)
	)
	variables_reversed <- variables[length(variables):1]

	# Get all possible data realizations, given strategies in which some data
	# is not sought (NA)
	observable <- lapply(
		variables,
		function(x) c(0,1,NA)
	)
	all_data <- do.call(expand.grid,observable)
	names(all_data) <- variables

	# Reorder the data so that strategies are grouped together and NAs increase
	order_list <-
		c(
			list(rowSums(is.na(all_data))),
			lapply(
				variables_reversed,
				function(variable) is.na(all_data[,variable])
			),
			lapply(
				variables_reversed,
				function(variable) all_data[,variable]
			)
		)
	all_data <- all_data[do.call(what = order,args = order_list),]

	# Remove strategy in which no data is sought
	all_data <- all_data[-nrow(all_data), ]

	# Figure out what strategy is being used in each of the possible data
	# realizations, given which nodes are unobserved
	which_strategy <- apply(
		X = all_data,
		MARGIN = 1,
		FUN = function(row){
			variables[!is.na(row)]
		})
	# Get the unique strategies of data collection
	strategies <- unique(which_strategy)
	# Get the number of unique strategies of data collection
	n_strategies <- length(strategies)
	# Now for each strategy, get the data that corresponds to it
	# this will be used in the data to fit a likelihood separately for
	# each strategy
	indices <-
		sapply(sapply(strategies,paste,collapse = ""),
					 function(x) which(sapply(which_strategy,paste,collapse = "") %in% x))
	# Get the indices
	starts <- sapply(indices,min)
	ends <- sapply(indices,max)
	# Now built the ambiguity matrix for the event probs as a function of the
	# fundamental data events (where all nodes are observed)
	all_data_labels <- all_data
	all_data_labels[is.na(all_data_labels)] <- ""

	# Find the unique realizations with variable labels
	data_realizations <- apply(
		X = all_data_labels,
		MARGIN = 1,
		FUN = function(row){
			variable_labels <- variables[!row == ""]
			row_subset <- row[!row == ""]
			paste(variable_labels,row_subset,sep = "")
		})
	# Get the realizations of the fundamental data events
	fundamental_data <- data_realizations[starts[1]:ends[1]]
	# For each data realization, find the fundamental data event in which those
	# same events would have been observed, and make a matrix of 1s and 0s
	A_w <- t(sapply(
		data_realizations,
		function(realization){
			sapply(fundamental_data,function(fun_data)all(realization %in% fun_data))
		}
	)) * 1

	rownames(A_w) <- 1:nrow(A_w)

	return(list(
		A_w = A_w, w_starts = starts, w_ends = ends, n_strategies = n_strategies
	))

}



