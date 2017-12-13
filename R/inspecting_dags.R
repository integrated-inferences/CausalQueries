
#' Create list by endogenous variable containing all chains terminating at this endogenous variable
#'
#' FUNCTION_DESCRIPTION
#'
#' @param dag DESCRIPTION.
#'
#' @return RETURN_DESCRIPTION
#' @examples
#' \dontrun{
#' test_dag <-
#' gbiqq::make_dag(add_edges(parent = "X",children = c("K", "Y1")),
#' 								add_edges(parent = "K", children = c("Y1","Y2")),
#' 								add_edges(parent = "Z", children = c("K")))
#'
#' get_chains(test_dag)
#' }
#'
#' @export
get_chains <- function(dag) {

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

	return(chains_out)
}




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
#' \dontrun{
#' dag <- make_dag(
#'  add_edges(parent = "X",children = c("K","Y")),
#'  add_edges(parent = "K",children = "Y")
#' )
#'
#' get_parents(dag)
#' }
#'

get_parents <- function(dag) {

	chains <- get_chains(dag)
	exogenous <- get_exogenous_vars(dag)

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
# get_parents <- function(dag){
# 	if(nrow(dag)>1){
# 		dag <- as.data.frame(apply(dag,2,as.character))
# 	}
# 	variables <- as.character(
# 		unique(
# 			unlist(dag)
# 		)
# 	)
# 	parents <- lapply(
# 		X = variables,
# 		FUN = function(variable) as.character(dag$parent[dag$child == variable])
# 	)
# 	names(parents) <- variables
#
# 	# Re-order by seniority
# 	n_parents <- sapply(parents,length)
#
# 	variables <- variables[order(n_parents)]
# 	parents <- parents[variables]
#
# 	return(parents)
# }



#' Get list of types for variables in a DAG
#'
#' @param dag A dag created by make_dag()
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
get_types <- function(dag) {

	types <- lapply(
		X = lapply(get_parents(test_dag), length),
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
#' @param dag A dag created by make_dag()
#'
#' @export
#'
#' @return A vector of exogenous variables
get_exogenous_vars <- function(dag){
	parents <- unique(dag$parent)
	return(
		as.character(parents[!(parents %in% dag$children)])
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
#' @export
#'
get_terminal_vars <- function(dag){
	children <- unique(dag$children)
	return(
		as.character(children[!(children %in% dag$parent)])
	)
}

#' Get endogenous variables in a DAG
#'
#' @param dag A dag created by make_dag()
#'
#' @export
#'
#' @return A vector of endogenous variables
get_endogenous_vars <- function(dag){
	return(
		c(setdiff(as.character(unique(dag$children)), get_terminal_vars(dag)), get_terminal_vars(dag))
	)
}


#' Get variable names from a DAG
#'
#' @param dag A dag created by make_dag()
#'
#' @export
#'
#' @return A vector of variable names
get_variables <- function(dag){
	return(
		c(get_exogenous_vars(dag), get_endogenous_vars(dag))
	)
}


#' Get observable data implied by DAG
#'
#' @param variables A dag created by make_dag()
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
#' \dontrun{
#' perm(c(2,2,2))
#' }
perm <- function(v) {
	sapply(1:length(v), function(x) {
		rep(rep(1:v[x], each = prod(v[x:length(v)])/v[x]), length.out = prod(v))
	}) - 1
}














