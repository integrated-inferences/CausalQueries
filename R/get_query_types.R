#' Look up query types
#'
#' Find the nodal or causal types used by the query. Passes arguments into \code{\link{map_query_to_causal_type}} or \code{\link{map_query_to_nodal_type}}
#'
#' @inheritParams CausalQueries_internal_inherit_params
#' @param map Types in query. Either \code{nodal_type} or \code{causal_type}. Default is causal types
#'
#' @export
#' @examples
#' model <- make_model('X -> M -> Y; X->Y')
#' query <- '(Y[X=0] > Y[X=1])'
#' get_query_types(model, query, map="nodal_type")
#' get_query_types(model, query, map="causal_type")
#' get_query_types(model, query)


get_query_types <- function(model, query, map = "causal_type"){
	if(!(map%in%c("causal_type", "nodal_type"))){
		stop("map must be either causal_type or nodal_type")
	}else if (map=="causal_type"){
		map_query_to_causal_type(model, query)
	}else{
		map_query_to_nodal_type(model, query)
	}
}


