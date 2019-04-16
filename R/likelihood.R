#' get_likelihood_helpers
#'
#' Produce the indices and ambiguity matrix needed for the
#'
#' @param dag A dag created by make_dag()
#'
#' @return Returns indices and ambiguity matrix
#'
#' @export


get_likelihood_helpers <- function(pcm){

	# Get variables in order of exogeneity
	variables <- c(
		gbiqq::get_exogenous_vars(pcm),
		gbiqq::get_endogenous_vars(pcm)
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


	# make names for all_data
	.all_data <- all_data
	.all_data[is.na(.all_data)] <- "*"
	for(j in 1:ncol(.all_data)) {
		.all_data[.all_data[ , j ]==0 , j ] <- tolower(names(.all_data[j]))
		.all_data[.all_data[ , j ]==1 , j ] <- toupper(names(.all_data[j]))}
	#rownames(all_data) <- 	apply(.all_data, 1, paste, collapse= "")

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
	# do.call(c,strategies)
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
	max_possible_data <- get_max_possible_data(pcm)
	A <- get_ambiguities_matrix(pcm)
	fundamental_data <- sapply(1:ncol(max_possible_data), function(i) paste0(colnames(max_possible_data)[i],max_possible_data[,i] ))
	fundamental_data <- matrix(	fundamental_data, ncol = ncol(max_possible_data))

	fundamental_data	<- lapply(1:nrow(fundamental_data),  function(i) c(fundamental_data[i,]))
  names(fundamental_data) <- rownames(A)
	# For each data realization, find the fundamental data event in which those
	# same events would have been observed, and make a matrix of 1s and 0s
	A_w <- t(sapply(
		data_realizations,
		function(realization){
			sapply(fundamental_data, function(fun_data)all(realization %in% fun_data))
		}
	)) * 1


	c_names <- sapply(	fundamental_data  , paste0, collapse = "")
	A_w <- matrix(A_w, ncol = length(c_names) )
	colnames(A_w) <- c_names
	possible_events <- sapply(data_realizations,paste,collapse = "")
	strategy_labels <- sapply(which_strategy,paste,collapse = "")
	names(possible_events) <- strategy_labels

	rownames(A_w) <- possible_events

	return(list(
		A_w = A_w,
		w_starts = starts,
		w_ends = ends,
		n_strategies = n_strategies,
		possible_events = possible_events
	))

}





#' new_get_likelihood_helpers
#'
#' Produce the indices and ambiguity matrix needed for the
#'
#' @param dag A dag created by make_dag()
#'
#' @return Returns indices and ambiguity matrix
#'
#'
#' @export

new_get_likelihood_helpers <- function(pcm){

}










