#' Get endogenous variables in a DAG which are also terminal
#'
#' FUNCTION_DESCRIPTION
#'
#' @param dag DESCRIPTION.
#'
#' @return RETURN_DESCRIPTION
#'
#' @export
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
#' @export
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

#' Get list of parents in a dag
#'
#' @param A probabilistic causal model created by make_model()
#'
#' @export
#'
#' @return A list of parents in a DAG
#'
#' @examples
#'
#' \dontrun{
#' dag <- make_dag(
#'  add_edges(parent = "X",children = c("K","Y")),
#'  add_edges(parent = "K",children = "Y")
#' )
#'
#' get_parents(dag)
#' }
#'
#' dag <- make_dag(
#'  add_edges(parent = "X",children = c("K")),
#'  add_edges(parent = "K",children = "Y")
#' )
#'
#' get_parents(dag)
#' get_ancestors(dag)


get_parents <- function(model) {
	dag <- model$dag
	sapply(paste(unique(unlist(dag))), function(j) paste(dag$parent)[paste(dag$children) == j])
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
#' @export
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
#' @export
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
#' @export
#'
#' @return A vector of variable names
get_variables <- function(model){

	return(
		c(get_exogenous_vars(model), get_endogenous_vars(model))
	)
}


#' Get list of types for variables in a DAG
#'
#' @param A probabilistic causal model created by make_model()
#'
#' @export
#'
#' @return A list of parents in a DAG
#'
#' @examples
#' \dontrun{
#' dag <- make_dag(
#'  add_edges(parent = "X",children = c("K","Y")),
#'  add_edges(parent = "K",children = "Y")
#' )
#'
#' get_types(dag)
#' }
#'
get_nodal_types <- function(model, collapse = TRUE) {
nodal_types <- model$nodal_types
variables <- get_variables(model)
	dag <- model$dag
	types <- lapply(
		X = lapply(get_parents(model), length),
		FUN = function(parent_n){
			type_mat <- perm(rep(2,2^parent_n))
			if(parent_n == 0){
				labels <- NULL
			} else {
				input_mat <- perm(rep(2,parent_n))
				labels <- apply(input_mat,1,paste,collapse = "")
			}
			colnames(type_mat) <- labels
			return(type_mat)
		})

	types_labels <- lapply(1:length(types), function(i){
		var <- names(types)[i]
		mat <- types[[i]]
		labels <- apply(mat,1,paste,collapse = "")
		paste0(var, labels)
	})

	names(types_labels )<- var_names <- names(types)
  types <- lapply(variables, function(v){
  	rownames(types[[v]]) <- types_labels[[v]]
  	types[[v]]
  })
  names(types)  <- var_names
  if(!is.null(nodal_types)){
  	types <- lapply(variables, function(v){
  		mat <- types[[v]]
  		cn <- colnames(mat)
  		nt <- nodal_types[[v]]
  		mat <- mat[nt, ]
  		colnames(mat) <- cn
  		mat
  	})
  }
  names(types)  <- var_names
  if(collapse){

  types <-	sapply(1:length(types), function(i){
  		var <- names(types)[i]
  		mat <- as.matrix(types[[i]])
  		labels <- apply(mat,1,paste,collapse = "")
  		paste0(var, labels)
  	})

  }

  names(types)  <- var_names

	return(types)
}
#' Get nodal types
#' Nodal types are created by concatening variables and their possible data. Used for labeling ambiguities matrix.
#'
#' @param model A dag as created by \code{make_dag}
#'
#' @return types
#' @export
#'
get_nodal_types_dep <- function(model){
	if(!is.null(model$nodal_types )){
		return(model$nodal_types )
	} else{


		parents <- get_parents(model)

		possible_data <-	get_possible_data(model, collapse = FALSE)
		nodes <- names(possible_data)

		return(mapply(function(realization, node) paste0(node, realization),
									node = nodes,
									realization = possible_data ))
	}
}


#' Get expanded types
#'
#' Create a data frame with types produced from all combinations of possible data produce by a dag.
#'
#' @param model A dag as created by \code{make_dag}
#'
#' @return types
#' @export
#'
get_expanded_types <- function(model){
	possible_types<-	get_nodal_types(model)
	variables <- names(possible_types)
	possible_types <- lapply(variables, function(v) gsub(v, "", possible_types[[v]]))
	names(possible_types) <- variables
	# Get types as the combination of nodal types/possible_data. for X->Y: X0Y00, X1Y00, X0Y10, X1Y10...
	expand.grid(possible_types, stringsAsFactors = FALSE)
}


#' Produces the possible permutations of a set of variables
#'
#' @param v A vector of integers indicating the number of values each variable takes on. E.g., a binary variable is represented by 2.
#'
#' @export
#'
#' @return A matrix of permutations
#'
#' @examples
#'
#' \dontrun{
#' perm(c(2,2,2))
#' }
perm <- function(v) {
	sapply(1:length(v), function(x) {
		rep(rep(1:v[x], each = prod(v[x:length(v)])/v[x]), length.out = prod(v))
	}) - 1
}

