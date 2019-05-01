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

	possible_data   <-	get_possible_data(model)
	nodes           <- names(possible_data)
	nodal_types     <- get_nodal_types(model)
	types           <- expand.grid(nodal_types, stringsAsFactors = FALSE)
	row_identifiers <- unlist(nodal_types)

	# Which nodal_types correspond to a type
	P <- sapply(1:nrow(types), function(i){
		type <- types[i,]
		sapply(row_identifiers, function(nodal_type)
			all(nodal_type %in% type) )})*1

	colnames(P)  <- do.call(paste, c(types, sep =""))
	rownames(P ) <- row_identifiers
	P
}


