#' Get nodal types
#' Nodal types are created by concatening variables and their possible data. Used for labeling ambiguities matrix.
#'
#' @param model A model created by \code{make_model}
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



#' @export
print.nodal_types <- function(x, ...) {
	print(summary(x))
	invisible(x)
}


#' @export
summary.nodal_types <- function(object, ...) {
	structure(object, class = c("summary.nodal_types", "data.frame"))

}

#' @export
print.nodal_types <- function(x, ...){

}
