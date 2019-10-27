#' Make parameter matrix
#'
#' Calculate parameter matrix assuming no confounding. The parameter matrix  maps from parameters into causal types. In models without confounding parameters correspond to nodal types.
#'
#' @param model A model created by make_model()
#' @export
#' @examples
#' model <- make_model("X -> Y")
#' make_parameter_matrix(model)

make_parameter_matrix  <- function(model){

	nodal_types     <- get_nodal_types(model)
#	param_set       <- unlist(mapply(function(a,b) rep(a,b), names(nodal_types), lapply(nodal_types, length), SIMPLIFY = FALSE))
#	param_set       <- 	model$parameters_df$param_set
	types           <- gbiqq:::causal_type_names(get_causal_types(model))
	pars            <- unlist(nodal_types)

	# Which nodal_types correspond to a type
	P <- sapply(1:nrow(types), function(i){
		type <- types[i,]
		sapply(pars, function(nodal_type)
			all(nodal_type %in% type) )})*1

	P <- sapply(1:nrow(types), function(i){
		type <- types[i,]
		sapply(pars, function(nodal_type)
			all(nodal_type %in% type) )})*1

  # Tidy up
	colnames(P) <- do.call(paste, c(types, sep ="."))
	rownames(P) <- model$parameters_df$param_names

	# Add the parameter set as attribute
	# names(param_set) <- NULL
	# attr(P, "param_set") <- param_set

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
	class(model$P) <- c("parameter_matrix")
	model
}


#' @export
print.parameter_matrix <- function(x, ...) {
	print(summary(x))
	invisible(x)
}


#' @export
summary.parameter_matrix <- function(object, ...) {
	structure(object, class = c("summary.parameter_matrix", "table"))

}

#' @export
print.summary.parameter_matrix <- function(x, ...){
  print("Rows are parameters, grouped in parameter sets")
	print("Columns are causal types")
	print("Cell entries indicate whether a parameter probability is used in the calculation of causal type probability")
	print.table(x)
	param_set <- attr(x,"param_set")

  cat("\n parameter set  (P)\n ")
  cat(paste0(param_set, collapse = "  "))
	if(!is.null(attr(x,"confounds"))){
		cat("\n\n confounds (P)\n")
		print(attr(x,"confounds"))
	}
}



# Names for causal types

causal_type_names <- function(causal_types) {
          for(j in (1:ncol(causal_types))) causal_types[,j] <- paste0(names(causal_types)[j], causal_types[,j])
					data.frame(causal_types, stringsAsFactors = FALSE)}

