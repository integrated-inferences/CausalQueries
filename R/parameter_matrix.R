#' Make parameter matrix
#'
#' Calculate parameter matrix assuming no confounding. The parameter matrix  maps from parameters into causal types. In models without confounding parameters correspond to nodal types.
#'
#' @param model A model created by make_model()
#'
#' @export
#'
make_parameter_matrix  <- function(model){

	possible_data   <- get_possible_data(model)
	nodes           <- names(possible_data)
	nodal_types     <- get_nodal_types(model)
	param_set       <- unlist(mapply(function(a,b) rep(a,b), names(nodal_types), lapply(nodal_types, length)))
	types           <- expand.grid(nodal_types, stringsAsFactors = FALSE)
	row_identifiers <- unlist(nodal_types)

	# Which nodal_types correspond to a type
	P <- sapply(1:nrow(types), function(i){
		type <- types[i,]
		sapply(row_identifiers, function(nodal_type)
			all(nodal_type %in% type) )})*1

	colnames(P)  <- do.call(paste, c(types, sep =""))
	rownames(P ) <- row_identifiers
	attr(P, "param_set") <- param_set
	P
}

#' Get parameter matrix
#'
#' Return parameter matrix if it exists; otherwise calculate it assuming no confounding. The parameter matrix  maps from parameters into causal types. In models without confounding parameters correspond to nodal types.
#'
#' @param model A model created by make_model()
#'
#' @export
#'
get_parameter_matrix  <- function(model){

	if(!is.null(model$P)) return(model$P)
  return(make_parameter_matrix(model))
}


#' Set parameter matrix
#'
#' Add a parameter matrix to a model
#'
#' @param model A model created by make_model()
#' @param P A parameter matrix
#'
#' @export
#'
set_parameter_matrix <- function(model, P = NULL){

	if(is.null(P)) P <- make_parameter_matrix(model)
	model$P <- P
	message("Parameter matrix attached to model")
	model
}


