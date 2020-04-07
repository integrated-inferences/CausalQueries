#' Look up query types
#'
#' Find which nodal or causal types are satisfied by a query.
#' @inheritParams CausalQueries_internal_inherit_params
#' @param map Types in query. Either \code{nodal_type} or \code{causal_type}. Default is \code{causal_type}.
#'
#' @export
#' @examples
#' model <- make_model('X -> M -> Y; X->Y')
#' query <- '(Y[X=0] > Y[X=1])'
#' get_query_types(model, query, map="nodal_type")
#' get_query_types(model, query, map="causal_type")
#' get_query_types(model, query)
#'
#' # Examples with map = "nodal_type"
#'
#' query <- '(Y[X=0, M = .] > Y[X=1, M = 0])'
#' get_query_types(model, query, map="nodal_type")
#'
#' query <- '(Y[] == 1)'
#' get_query_types(model, query, map="nodal_type")
#' get_query_types(model, query, map="nodal_type", join_by = '&')
#'
#' # Root nodes specified with []
#' get_query_types(model, '(X[] == 1)', map="nodal_type")
#'
#' query <- '(M[X=1] == M[X=0])'
#' get_query_types(model, query, map="nodal_type")
#'
#' # Helpers
#' model <- make_model('M->Y; X->Y')
#' query <- complements('X', 'M', 'Y')
#' get_query_types(model, query, map="nodal_type")
#'
#' # Examples with map = "causal_type"
#'
#' model <- make_model('X -> M -> Y; X->Y')
#' query <- 'Y[M=M[X=0], X=1]==1'
#' get_query_types(model, query, map= "causal_type")
#'
#' query <- '(Y[X=1, M = 1] >  Y[X=0, M = 1]) & (Y[X=1, M = 0] >  Y[X=0, M = 0])'
#' get_query_types(model, query, "causal_type")
#'
#' query <- 'Y[X=1] == Y[X=0]'
#' get_query_types(model, query, "causal_type")
#'
#' query <- '(X == 1) & (M==1) & (Y ==1) & (Y[X=0] ==1)'
#' get_query_types(model, query, "causal_type")
#'
#' query <- '(Y[X = .]==1)'
#' get_query_types(model, query, "causal_type")
#'
get_query_types <- function(model, query, map = "causal_type", join_by = "|"){
	if(!(map%in%c("causal_type", "nodal_type"))){
		stop("map must be either causal_type or nodal_type")
	}else if (map=="causal_type"){
		map_query_to_causal_type(model, query, join_by)
	}else{
		map_query_to_nodal_type(model, query, join_by)
	}
}


