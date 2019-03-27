#' FUNCTION_TITLE
#'
#' FUNCTION_DESCRIPTION
#'
#' @param dag DESCRIPTION.
#'
#' @return RETURN_DESCRIPTION
#' @examples
#' # ADD_EXAMPLES_HERE
#' @export
make_adjacency_matrix <- function(dag) {
	vars <- gbiqq::get_variables(dag)

	adj_matrix <-
		matrix(FALSE,
					 nrow = length(vars),
					 ncol = length(vars))

	colnames(adj_matrix) <- rownames(adj_matrix) <- vars

	for (i in 1:nrow(dag)) {
		adj_matrix[as.character(dag$children[i]), as.character(dag$parent[i])] <- TRUE
	}

	return(adj_matrix)

}

# collapse_ambiguity_matrices <- function(dag) {
#
# 	## define some useful objects for later
# 	endogenous <- gbiqq::get_endogenous_vars(dag)
# 	exogenous <- gbiqq::get_exogenous_vars(dag)
# 	ambiguity_matrices <- make_ambiguity_matrices(dag)
# 	possible_data <- gbiqq::get_possible_data(dag, collapse = FALSE)
# 	# parents <- gbiqq::get_parents(dag)
# 	endogenous_adj_matrix <- gbiqq::make_adjacency_matrix(dag)[endogenous,endogenous, drop = FALSE]
#
# 	# ## create adjacency matrix for endogenous variables only
# 	# endogenous_adj_matrix <-
# 	# 	sapply(endogenous,
# 	# 				 FUN = function(endog) {
# 	# 				 	sapply(parents[endogenous],
# 	# 				 				 FUN = function(parents) { endog %in% parents })
# 	# 				 })
# 	#
#
# 	## collapse endogenous_adj_matrix to list with complex structure:
# 	## Each element of list is character string and corresponds to
# 	## terminal child (thus no endogenous terminal childs is prohibited)
# 	## From terminal child we either (a) create list element consisting oа its name
# 	## (if there are no endogenous parents of that child)
# 	## or (b) create character vector consisting of all direct and indirect endogenous parents of the node
# 	## Purpose: use this to create endogenous collapsed list elements
# 	collapse_list <- list()
#
# 	for (i in seq_along(endogenous)) {
#
# 		parent_of <- endogenous_adj_matrix[,endogenous[i]]
# 		child_of <- endogenous_adj_matrix[endogenous[i],]
#
# 		if (!any(c(parent_of, child_of))) {
# 			collapse_list[[endogenous[i]]] <- endogenous[i]
# 		} else if (any(child_of) & !any(parent_of)) {
# 			collapse_list[[endogenous[i]]] <- c()
# 			child_of_temp <- names(child_of)[which(child_of)]
# 			while (!is.null(child_of_temp)) {
# 				collapse_list[[endogenous[i]]] <-
# 					union(child_of_temp, collapse_list[[endogenous[i]]])
# 				child_of_temp <-
# 					unlist(sapply(child_of_temp,
# 												FUN = function(child) {
# 													names(endogenous_adj_matrix[child_of_temp,])[which(endogenous_adj_matrix[child_of_temp,])]
# 												}))
# 			}
# 		}
# 	}
#
# 	## now comes tedious technical part: Given collapse_list merge in all endogenous parents into
# 	## terminal child and create output object for endogenous variables
# 	## COLLAPSED AMBIGUITY HAHA
# 	collapsed_ambiguity <- list()
#
# 	for (i in seq_along(collapse_list)) {
#
# 		if (length(collapse_list[[i]]) == 1 & names(collapse_list)[i] %in% collapse_list[[i]]) {
# 			# if name of collapse element matches the string row than there are no endogenous parents or childs to the variable
# 			collapsed_ambiguity[[names(collapse_list)[i]]] <- ambiguity_matrices[[names(collapse_list)[i]]]
# 		} else {
# 			# else we have to go through all endogenous parents of the terminal child and merge them into child's
# 			# ambiguity matrix
# 			collapsed_ambiguity[[ paste0(c(collapse_list[[i]], names(collapse_list)[i]), collapse = "_") ]] <-
# 				ambiguity_matrices[[names(collapse_list)[i]]]
#
# 			for (j in seq_along(collapse_list[[i]])) {
#
# 				# create helper objects so that code is more readable
# 				.parent_data <- possible_data[[collapse_list[[i]][j]]]
# 				.parent_ambiguity <- ambiguity_matrices[[collapse_list[[i]][j]]]
# 				.child_data <- possible_data[[names(collapse_list)[i]]]
# 				.child_ambiguity <- collapsed_ambiguity[[ paste0(c(collapse_list[[i]], names(collapse_list)[i]), collapse = "_") ]]
#
# 				# create matrixes mergable by possible data columns
# 				# (only parent columns, since they all have to be in child!)
# 				endogenous_parent <- cbind(.parent_data, .parent_ambiguity)
# 				endogenous_child <- cbind(.child_data, .child_ambiguity)
#
#
# 				# inner join (modified) terminal parent and child to create temporary matrix with:
# 				# 1. all possible_data columns for terminal child
# 				# 2. all terminal child ambiguity values
# 				# 3. merged in parent ambiguity values
# 				endogenous_parent_child <-
# 					merge(x = endogenous_child,
# 								y = endogenous_parent,
# 								by = colnames(.parent_data),
# 								suffixes = c("", "_parent"),
# 								sort = F)
#
# 				# here comes the tricky part:
# 				# create expanded matrix in the right ambiguity format from the
# 				# parent_child object created above
# 				collapsed <-
# 					t(
# 						apply(endogenous_parent_child, MARGIN = 1,
# 									FUN = function(row) {
# 										# child ambiguity values
# 										.row_child <- row[(ncol(.child_data) + 1):ncol(endogenous_child)]
# 										# parent ambiguity values
# 										.row_parent <- row[(ncol(endogenous_child) + 1):ncol(endogenous_parent_child)]
# 										# create df with all possible combinations of parent and child
# 										.grid_df <- expand.grid(parent = 1:ncol(.parent_ambiguity),
# 																						child = 1:ncol(.child_ambiguity))
# 										# multiply
# 										return(.row_child[.grid_df$child] * .row_parent[.grid_df$parent])
# 									}
# 						))
#
# 				if (any(dim(collapsed) !=
# 								c(nrow(.child_data),
# 									ncol(.parent_ambiguity) * ncol(.child_ambiguity)))) {
# 					stop("Mismatch in collapsed object and parent/child combination")
# 				}
#
# 				# name the collapsed object using the "_" as a separator and inheriting row names
# 				# note that row order changes due to merge behavior in creation of endogenous_parent_child
# 				rownames(collapsed) <- apply(endogenous_parent_child[,colnames(.child_data)],
# 																		 MARGIN = 1,
# 																		 FUN = paste, collapse = "")
# 				colnames(collapsed) <- apply(expand.grid(parent = colnames(.parent_ambiguity),
# 																								 child = colnames(.child_ambiguity)),
# 																		 MARGIN = 1,
# 																		 FUN = paste, collapse = "_")
#
# 				collapsed_ambiguity[[ paste0(c(collapse_list[[i]], names(collapse_list)[i]), collapse = "_") ]] <- collapsed
#
# 			}
#
# 		}
#
# 	}
#
# 	return(
# 		append(ambiguity_matrices[exogenous], collapsed_ambiguity)
# 	)
#
# }



#' FUNCTION_TITLE
#'
#' FUNCTION_DESCRIPTION
#'
#' @param dag DESCRIPTION.
#'
#' @return RETURN_DESCRIPTION
#'
#' @export
#'
#' @examples
#' # ADD_EXAMPLES_HERE
expand_ambiguity_matrices_internal <- function(dag) {

	max_possible_data <- get_max_possible_data(dag)

	matrices_out <-
		mapply(possible_data = get_possible_data(dag, collapse = FALSE)[get_endogenous_vars(dag)],
					 ambiguity = make_ambiguity_matrices(dag)[get_endogenous_vars(dag)],
					 FUN = function(possible_data, ambiguity) {
					 	out <- merge(max_possible_data, cbind(possible_data,ambiguity), all.x = TRUE)
					 	out <- out[do.call("order", as.list(out[,rev(names(max_possible_data))])),]
					 	if (!all(max_possible_data == out[,names(max_possible_data)]))
					 		stop("The naming of rows is an issue in expand_ambiguity_matrices_internal.")
					 	rownames(out) <- apply(out[,names(max_possible_data)], 1, paste0, collapse = "")
					 	out <- out[,!(names(out) %in% names(max_possible_data))]
					 	return(out)
					 },
					 SIMPLIFY = FALSE)

	return(matrices_out)

}


#' FUNCTION_TITLE
#'
#' FUNCTION_DESCRIPTION
#'
#' @param dag DESCRIPTION.
#'
#' @return RETURN_DESCRIPTION
#'
#' @export
#'
#' @examples
#' # ADD_EXAMPLES_HERE
expand_ambiguity_matrices <- function(dag) {

	ambiguity_ls <-
		lapply(expand_ambiguity_matrices_internal(dag),
					 FUN = function(mat) {
					 	lapply(split(mat, f = seq(nrow(mat))), FUN = unlist)
					 })

	expand_grid_fn <- function(...) { Reduce(x = expand.grid(...), f = "*") }

	out <- t(do.call(mapply, append(list(FUN = expand_grid_fn), ambiguity_ls)))

	max_possible_data <- get_max_possible_data(dag)

	rownames(out) <- apply(max_possible_data, 1, paste0, collapse = "")
	colnames(out) <-
		Reduce(x = lapply(gbiqq::get_types(dag)[get_endogenous_vars(dag)],
											FUN = function(types) as.character(1:nrow(types))),
					 f = function(a,b) {
					 	apply(expand.grid(x = a, y = b), 1, paste0, collapse = "_")
					 })

	return(out)

}

#' FUNCTION_TITLE
#'
#' FUNCTION_DESCRIPTION
#'
#' @param dag DESCRIPTION.
#'
#' @return RETURN_DESCRIPTION
#'
#' @export
#'
#' @examples
#' # ADD_EXAMPLES_HERE
get_ambiguity_matrix <- function(dag){

  # 1. Get nodal types. e.g. for X->Y: X0, X1, Y00, Y01..
	possible_data <-	get_possible_data(dag)
	nodes <- names(possible_data)
	nodal_types <-  mapply(function(realization, node) paste0(node, realization),
												 node = nodes,
												 realization = possible_data )


	# 2. Get types as the combination of nodal types. e.g. for X->Y: X0Y00, X1Y00, X0Y10, X1Y10...
	types <- expand.grid(possible_data, stringsAsFactors = FALSE)

	# 3. types_to_data is a helper object for getting ambiguities
	##  It matches types to data realization
	##  e.g type =  X0Y00 & type = X0Y10 -> types_to_data = X0 Y0
   nodal_types_to_data <-  mapply(function(realization, node){
		    										var_value <- substr(realization, nchar(realization) , nchar(realization))
		     									  paste0(node, var_value)
		     									},
												  node = nodes,
												  realization = possible_data )

	 types_to_data <- expand.grid(nodal_types_to_data, stringsAsFactors = FALSE)


  # 4. Get max possible data events and, in order to do matching, express it as X0 Y0 instead of 00
	max_possible_data <- get_max_possible_data(dag)
	data_events <- sapply(1:ncol(max_possible_data), function(j) paste0(names(max_possible_data)[j], max_possible_data[,j] ))


  # 5. Create A: Matrix that maps types to data events
	A <- sapply(1:nrow(col_identifiers), function(i){
							type <- col_identifiers[i,]
							sapply(1:nrow(data_events), function(j){
											event <- data_events[j,]
											all(event %in% type)
						  			})
							}*1)

  #6. Name cols and rows


	colnames(A) <- do.call(paste, c(types, sep ="."))
	rownames(A) <- apply(data_events, 1, paste0, collapse = "")

	return(A)
}

#' Reveal data
#'
#' Reveals data given types of exogenous variables
#'
#' @param dag A dag as created by \code{make_dag}
#'
#' @return revealed_data
#' @keywords internal
#'
#' @examples
#' # ADD_EXAMPLES_HERE
reveal_data <- function(dag){
	# 1. Get nodal types. e.g. for X->Y: X0, X1, Y00, Y01..
	possible_data <-	get_possible_data(dag)
	parents_list <- get_parents(dag)
	# 2. Get types as the combination of nodal types. e.g. for X->Y: X0Y00, X1Y00, X0Y10, X1Y10...
	types <- expand.grid(possible_data, stringsAsFactors = FALSE)

	exogenous_vars <- get_exogenous_vars(dag)
	endogenous_vars <- get_endogenous_vars(dag)

	types_of_exogenous <-   data.frame(types[, exogenous_vars])
	names(types_of_exogenous) <- 	exogenous_vars
	types_of_endogenous <-  data.frame(types[, endogenous_vars])
	names(types_of_endogenous) <- 	endogenous_vars
	data_realizations <- 	types
	revealed_data <- 		sapply(1:ncol(types_of_endogenous), function(j) {
			var <- names(types_of_endogenous)[j]
			child_type <- types_of_endogenous[,j]
			parents <- parents_list[[var]]

			sapply(1:length(child_type), function(i){
    				type <- child_type[i]
	  				type_ <- unlist(strsplit(type, split = ""))
	  			 	outcome_index <- nchar(type)
	  			 	outcome <- type_[outcome_index]
	  			 	predecessors <- type_[1:(outcome_index-1)]
	  			 	affects <- predecessors != outcome
	  			 	causes <- parents[affects]
	  			 	value_of_causes <- as.numeric(data_realizations[i, parentt])
	  			 	revealed_outcome <- ifelse(all(!affects), outcome, prod(value_of_causes == outcome))
	  			 })})

	  data_realizations[, endogenous_vars] <- revealed_data
	  data_realizations
}


