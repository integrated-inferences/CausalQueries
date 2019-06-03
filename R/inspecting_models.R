
#' Get list of parents in a dag
#'
#' @param A probabilistic causal model created by make_model()
#'
#' @export
#' @return A list of parents in a DAG
#'
#' @examples
#'
#' model <- make_model("X -> K -> Y")
#' get_parents(model)

get_parents <- function(model) {
	dag <- model$dag
	sapply(paste(unique(unlist(dag))), function(j) paste(dag$parent)[paste(dag$children) == j])
}




#' Get causal types
#'
#' Create a data frame with types produced from all combinations of possible data produce by a dag.
#'
#' @param model A model as created by \code{make_model}
#'
#' @return types
#' @export
#'
get_causal_types <- function(model){
	if(!is.null(model$causal_types)){
		return_df <- model$causal_types
	} else {
		return_df <- update_causal_types(model)
	}
	return_df
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
