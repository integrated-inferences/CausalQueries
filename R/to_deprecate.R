### FROM PREPPING STAN
#' Higher level function that returns all of the type ambiguities for all possible patterns of evidence
#'
#' @param model A dag created by make_dag()
#'
#' @export
#'
#' @return A list of mappings between types
get_ambiguities <- function(model){

	variables <- get_variables(model)
	# parents <- get_parents(model)
	types <- get_nodal_types(model, collapse = FALSE)

	possible_data <- get_possible_data(model)

	ambiguities <- list()

	for (variable in variables) {
		ambiguities[variable][[1]] <- get_ambiguities_internal(
			sub_types = types[variable][[1]],
			possible_data = possible_data[variable][[1]]
		)
	}

	return(ambiguities)

}

get_ambiguities_internal <-
	function(sub_types, possible_data) {
		parent_values <- colnames(sub_types)
		realizations <- sub_types

		for (parval in parent_values) {
			realizations[,parval] <- paste0(parval,realizations[,parval])
		}

		type_ambiguities <- t(
			sapply(
				X = possible_data,
				FUN = function(x) {
					which(
						apply(
							X = realizations,
							MARGIN = 1,
							FUN = function(y) x %in% y
						)
					)
				}
			)
		)

		# If there are no parents, must coerce back to matrix
		if (all(dim(type_ambiguities) == c(1,2))) {
			type_ambiguities <- t(as.matrix(type_ambiguities))
		}
		return(type_ambiguities)
	}


#' Internal function that transforms the ambiguity list into an indicator
#' function
#' @param ambiguities Ambiguities matrix produced by \code{get_ambiguities()}
make_ambiguity_matrix_internal <- function(ambiguities){
	n_types <- max(ambiguities)

	amb_mat <- matrix(data = 0,nrow = nrow(ambiguities),ncol = n_types)

	for (i in 1:nrow(ambiguities)) {
		amb_mat[i,ambiguities[i,]] <- 1
	}

	return(amb_mat)

}

#' Higher level function that returns an indicator matrix showing correspondence between type and possible evidence pattern for each type and evidence pattern
#'
#' @param dag A dag created by make_dag()
#'
#' @export
#'
#' @return A list of mappings between types
make_ambiguity_matrices <- function(model){

	ambiguities <- get_ambiguities(model)
	possible_data <- get_possible_data(model)

	ambiguity_matrices <- list()
	for (variable in names(ambiguities)) {
		ambiguity_matrices[variable][[1]] <- make_ambiguity_matrix_internal(
			ambiguities = ambiguities[variable][[1]]
		)

		rownames(ambiguity_matrices[variable][[1]]) <- possible_data[variable][[1]]
		colnames(ambiguity_matrices[variable][[1]]) <- 1:ncol(ambiguity_matrices[variable][[1]])

	}
	return(ambiguity_matrices)
}


#' Map types of every varibale to observable data
#'
#' @param model A dag created by make_dag()
#'
#' @export
#'
#' @return A list of mappings between types and data
map_types_to_data <- function(model){

	possible_data <- get_possible_data(model)
	# types <- get_types(dag)
	variables <- get_variables(model)
	# type_indices <- get_type_indices(dag)
	parents <- get_parents(model)
	n_vars <- sapply(parents,length) + 1
	ambiguities <- get_ambiguities(model)

	# Get the data at the terminal node
	full_data <- possible_data[variables[length(variables)]][[1]]

	type_mapping <- list()
	for (variable in variables) {
		row_indices <- substr(full_data,start = 1,stop = n_vars[variable])
		type_map <- ambiguities[variable][[1]][row_indices,]
		if (is.null(dim(type_map))) dim(type_map) <- c(length(type_map),1)
		n_types <- max(type_map)
		amb_mat <- matrix(data = 0,nrow = nrow(type_map),ncol = n_types)
		for (i in 1:nrow(type_map)) {
			amb_mat[i,type_map[i,]] <- 1
		}
		rownames(amb_mat) <- full_data
		colnames(amb_mat) <- 1:n_types
		type_mapping[[variable]] <- amb_mat
	}

	return(type_mapping)

}

#' Get helpers for expanding the pi matrix
#'
#' @param model A dag created by make_dag()
#'
#' @export
#'
#' @return A list of pi helpers
get_pi_expanders <- function(pi,model){

	types <- get_n_endogenous_types(model)
	lapply(pi,
				 FUN = function(exogenous_var) {

				 	pos <- which(names(exogenous_var) == names(types))
				 	select_vars <-
				 		cumsum((seq_along(names(types)) == min(pos)) +
				 					 	(seq_along(names(types)) == (max(pos) + 1)))

				 	return(list(times = prod(types[select_vars == 2]),
				 							each = prod(types[select_vars == 0])))

				 })
}



## from inspecting_models


#' Create list by endogenous variable containing all chains terminating at this endogenous variable
#'
#' FUNCTION_DESCRIPTION
#'
#' @param model DESCRIPTION.
#'
#' @return RETURN_DESCRIPTION
#' @examples
#' \dontrun{
#' test_dag <-
#' gbiqq::make_model(add_edges(parent = "X",children = c("K", "Y1")),
#' 								add_edges(parent = "K", children = c("Y1","Y2")),
#' 								add_edges(parent = "Z", children = c("K")))
#'
#' get_chains(test_dag)
#' }
#'
#' @export
get_chains <- function(model) {
	dag <- model$dag
	n <- 0
	chains_out <- setNames(object = dag, nm = c("parent", paste0("children", n)))

	repeat {
		n <- n + 1
		temp_dag <- setNames(object = dag, nm = c("parent",paste0("children", n)))
		chains_out <-
			suppressWarnings(
				base::merge(temp_dag, chains_out,
										by.x = paste0("children", n), by.y = "parent", all.y = TRUE, suffixes = c("", n))
			)

		parents <- chains_out$parent
		if (all(is.na(parents))) {
			chains_out <- chains_out[,-which(names(chains_out) == "parent")]
			break
		}
	}

	chains_out <- split(chains_out, f = chains_out$children0)

	chains_out <-
		lapply(chains_out, FUN = function(df) {
			lapply(split(df, seq(nrow(df))), FUN = function(row) {unname(row[!is.na(row)])})
		})

	chains_out <- append(x = chains_out[!(names(chains_out) %in% get_terminal_vars(dag))],
											 values = chains_out[names(chains_out) %in% get_terminal_vars(dag)])

	return(chains_out)
}









#' Get observable data implied by DAG
#'
#' @param variables A dag created by make_dag()
#'
#'
#' @return A list of data realizations
get_observable_data <- function(variables){

	big_grid <- expand.grid(
		lapply(1:length(variables),function(x) variables )
	)
	big_grid <- t(apply(big_grid,1,function(x)x[order(match(x,variables))]))

	var_combns <- unique(apply(big_grid,1,function(x) paste(unique(x),collapse = "")))
	# var_combns <- unique(apply(big_grid,1,function(x) unique(x)))
	var_combns <- var_combns[order(nchar(var_combns))]

	observable_data <- lapply(nchar(var_combns),function(x){
		apply(expand.grid(lapply(1:x,function(i)0:1)),1,paste,collapse = "")
	})
	names(observable_data) <- var_combns
	return(observable_data)
}




#' Get number of endogenous types
#'
#' @param A probabilistic causal model created by make_model()
#'
#'
#' @return A list of the numbers of endogenous types
get_n_endogenous_types <- function(model){

	types <- sapply(gbiqq::get_types(model), FUN = function(types) dim(types)[1])
	types <- types[gbiqq::get_endogenous_vars(model)]
	return(types)
}



#' Get children of exogenous variables
#'
#' @param A probabilistic causal model created by make_model()
#'
#' @return A list of the children of exogenous variables
get_children_of_exogenous <- function(model){
	dag <- model$dag
	sapply(gbiqq::get_exogenous_vars(model),
				 FUN = function(exogenous_variable) {
				 	names(gbiqq::get_parents(model))[sapply(X = gbiqq::get_parents(model),
				 																					FUN = function(parents) exogenous_variable %in% parents ) ] },
				 simplify = FALSE
	)
}


#' Get endogenous variables in a DAG which are also terminal
#'
#' FUNCTION_DESCRIPTION
#'
#' @param dag DESCRIPTION.
#'
#' @return RETURN_DESCRIPTION
#'
#'
get_terminal_vars <- function(model){
	dag <- model$dag
	children <- unique(dag$children)
	return(
		as.character(children[!(children %in% dag$parent)])
	)
}




#' Get list of ancestor variables in a dag
#'
#' @param A probabilistic causal model created by make_model()
#'
#'
#' @return A list of ancestors in a DAG
#'
#' @examples
#'
#' \dontrun{
#' dag <- make_dag(
#'  add_edges(parent = "X",children = c("K","Y")),
#'  add_edges(parent = "K",children = "Y")
#' )
#'
#' get_ancestors(dag)
#' }
#'

get_ancestors <- function(model) {
	dag <- model$dag
	chains <- get_chains(model)
	exogenous <- get_exogenous_vars(model)

	endogenous_parents <-
		sapply(X = names(chains),
					 FUN = function(endog_name) {

					 	parents <- unlist(chains[[endog_name]])

					 	setdiff(x = unique(c(base::intersect(parents, exogenous),
					 											 base::setdiff(parents, exogenous))),
					 					y = endog_name)
					 })

	exogenous_parents <-
		sapply(exogenous,
					 FUN = function(vars) { character(0) },
					 simplify = FALSE)

	return(append(x = exogenous_parents,
								values = endogenous_parents))

}

## OLD VERSION
# get_types <- function(dag){
#
# 	parents <- get_parents(dag)
#
# 	n_variables <- length(parents)
#
# 	variable_names <- names(parents)
#
# 	n_parents <- sapply(parents,length)
#
# 	types <- lapply(
# 		X = n_parents,
# 		FUN = function(parent_n){
# 			type_mat <- perm(rep(2,2^parent_n))
# 			if(parent_n == 0){
# 				labels <- NULL
# 			} else {
# 				input_mat <- perm(rep(2,parent_n))
# 				labels <- apply(input_mat,1,paste,collapse = "")
# 			}
# 			colnames(type_mat) <- labels
# 			return(type_mat)
# 		}
# 	)
# 	return(types)
# }

#' Get exogenous variables in a DAG
#'
#' @param A probabilistic causal model created by make_model()
#'
#'
#' @return A vector of exogenous variables
get_exogenous_vars <- function(model){
	dag <- model$dag
	parents <- unique(dag$parent)
	return(
		as.character(parents[!(parents %in% dag$children)])
	)
}



#' Get endogenous variables in a DAG
#'
#' @param A probabilistic causal model created by make_model()
#'
#'
#' @return A vector of endogenous variables
get_endogenous_vars <- function(model){
	dag <- model$dag
	return(
		c(setdiff(as.character(unique(dag$children)), get_terminal_vars(model)), get_terminal_vars(model))
	)
}


#' Get variable names from a DAG
#'
#' @param A probabilistic causal model created by make_model()
#'
#'
#' @return A vector of variable names
get_variables <- function(model){

	return(
		c(get_exogenous_vars(model), get_endogenous_vars(model))
	)
}


