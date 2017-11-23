#' Get list of parent variables in a dag
#'
#' @param dag A dag created by make_dag()
#'
#' @export
#'
#' @return A list of parents in a DAG
#'
#' @examples
#'
#' dag <- make_dag(
#'  add_edges(parent = "X",children = c("K","Y")),
#'  add_edges(parent = "K",children = "Y")
#' )
#'
#' get_parents(dag)
#'
#'
get_parents <- function(dag){
	if(nrow(dag)>1){
		dag <- as.data.frame(apply(dag,2,as.character))
	}
	variables <- as.character(
		unique(
			unlist(dag)
		)
	)
	parents <- lapply(
		X = variables,
		FUN = function(variable) as.character(dag$parent[dag$child == variable])
	)
	names(parents) <- variables

	# Re-order by seniority
	n_parents <- sapply(parents,length)

	variables <- variables[order(n_parents)]
	parents <- parents[variables]

	return(parents)
}

#' Get list of types for variables in a DAG
#'
#' @param dag A dag created by make_dag()
#'
#' @export
#'
#' @return A list of parents in a DAG
#'
#' @examples
#'
#' dag <- make_dag(
#'  add_edges(parent = "X",children = c("K","Y")),
#'  add_edges(parent = "K",children = "Y")
#' )
#'
#' get_types(dag)
#'
#'
get_types <- function(dag){

	parents <- get_parents(dag)

	n_variables <- length(parents)

	variable_names <- names(parents)

	n_parents <- sapply(parents,length)

	types <- lapply(
		X = n_parents,
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
		}
	)
	return(types)
}

#' Get exogenous variables in a DAG
#'
#' @param dag A dag created by make_dag()
#'
#' @export
#'
#' @return A vector of exogenous variables
get_exogenous_vars <- function(dag){
	parents <- get_parents(dag)
	names(parents)[sapply(parents,length) == 0]
}

#' Get endogenous variables in a DAG
#'
#' @param dag A dag created by make_dag()
#'
#' @export
#'
#' @return A vector of endogenous variables
get_endogenous_vars <- function(dag){
	parents <- get_parents(dag)
	names(parents)[sapply(parents,length) > 0]
}


#' Get variable names from a DAG
#'
#' @param dag A dag created by make_dag()
#'
#' @export
#'
#' @return A vector of variable names
get_variables <- function(dag){
	names(get_parents(dag))
}


#' Get observable data implied by DAG
#'
#' @param dag A dag created by make_dag()
#'
#' @export
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
#' perm(c(2,2,2))
#'
perm <- function (v) {
	sapply(1:length(v), function(x) {
		rep(rep(1:v[x], each = prod(v[x:length(v)])/v[x]), length.out = prod(v))
	}) - 1
}














