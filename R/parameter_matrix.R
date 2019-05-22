#' Make parameter matrix
#'
#' Calculate parameter matrix assuming no confounding. The parameter matrix  maps from parameters into causal types. In models without confounding parameters correspond to nodal types.
#'
#' @param model A model created by make_model()
#' @param confound A list relating nodes to types with whihc  they are confounded
#'
#' @export
#' @examples
#' model <- make_model(add_edges(parent = "X", children = "Y"))
#' confound = list(ancestor = c(X="X"), descendent_type = list( Y = c("00","11") ))
#' make_parameter_matrix(model = model, confound = confound)
#' model <- make_model("X" %->% "M", "M" %->% "Y")
#' confound = list(ancestor = c(X="X"), descendent_type = list( Y = c("00","11") ))
#' make_parameter_matrix(model = model, confound = confound)

make_parameter_matrix  <- function(model, confound = NULL){

	nodal_types     <- get_nodal_types(model)
	param_set       <- unlist(mapply(function(a,b) rep(a,b), names(nodal_types), lapply(nodal_types, length)))
	types           <- expand.grid(nodal_types, stringsAsFactors = FALSE)
	pars            <- unlist(nodal_types)

	# Which nodal_types correspond to a type
	P <- sapply(1:nrow(types), function(i){
		type <- types[i,]
		sapply(pars, function(nodal_type)
			all(nodal_type %in% type) )})*1


	if(!is.null(confound)) {

		A  <- confound[[1]] # Ancestors
		D  <- confound[[2]] # Descendents

		if(length(A) != length(D)) stop("Please provide matched ancestor and descendent lists for confounded relations")

		Ds <- names(D)

  	for(j in 1:length(A)) {
  		# Make duplicate entries
			P             <- rbind(P[param_set == A[j],], P)

			new_params    <- paste0(pars[param_set == A[j]], "", paste0(Ds[j], "", paste(D[j][[1]], collapse = "_")))
			pars          <- c(new_params, pars)

			new_param_set <- paste0(param_set[param_set == A[j]], "", paste0(Ds[j], "", paste(D[j][[1]], collapse = "_")))
			param_set     <- c(new_param_set,param_set)


			# Zero out duplicated entries
			P[param_set == A[j], types[,names(D)[j]]	%in%	paste0(Ds[j], D[j][[1]])] <- 0
			P[param_set %in% new_param_set, !(types[, names(D)[j]]	%in%	paste0(Ds[j], D[j][[1]]))] <- 0
  	}
		attr(P, "confounds") <- data.frame(t(mapply(c, names(confound[[1]]), names(confound[[2]]))))
		}

  # Tidy up
	colnames(P)  <- do.call(paste, c(types, sep =""))
	rownames(P ) <- pars

	# Add the parameter set as attribute
	names(param_set) <- NULL
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
set_parameter_matrix <- function(model, P = NULL, confound = NULL){

	if(is.null(P)) P <- make_parameter_matrix(model, confound = confound)
	model$P <- P
	class(model$P) <- c("parameter_matrix")
	message("Parameter matrix attached to model")
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

  print.table(x)
	param_set <- attr(x,"param_set")

  cat("\n parameter set  (P)\n ")
  cat(paste0(param_set, collapse = "  "))
	if(!is.null(attr(x,"confounds"))){
		cat("\n\n confounds (P)\n")
		print(attr(x,"confounds"))
	}
}




