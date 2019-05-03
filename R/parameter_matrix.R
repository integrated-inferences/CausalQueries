#' Make parameter matrix
#'
#' Calculate parameter matrix assuming no confounding. The parameter matrix  maps from parameters into causal types. In models without confounding parameters correspond to nodal types.
#'
#' @param model A model created by make_model()
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

	possible_data   <- get_possible_data(model)
	nodes           <- names(possible_data)
	nodal_types     <- get_nodal_types(model)
	param_set       <- unlist(mapply(function(a,b) rep(a,b), names(nodal_types), lapply(nodal_types, length)))
	types           <- expand.grid(nodal_types, stringsAsFactors = FALSE)
	pars <- unlist(nodal_types)


	# Which nodal_types correspond to a type
	P <- sapply(1:nrow(types), function(i){
		type <- types[i,]
		sapply(pars, function(nodal_type)
			all(nodal_type %in% type) )})*1


	if(!is.null(confound)) {

		A  <- confound[[1]] # Ancestors
		D  <- confound[[2]] # Descendents
		Ds <- names(D)

  	for(j in 1:length(A)) {
			P         <- rbind(P[param_set == A[j],], P)
			new_params <- paste0(pars[param_set == A[j]], "-", paste0(Ds[j], ".", paste(D[j][[1]], collapse = ".")))
			pars       <- c(new_params, pars)
			param_set  <- c(new_params,param_set)
			P[param_set == A[j], types[,names(D)[j]]	%in%	paste0(Ds[j], D[j][[1]])] <- 0
			P[param_set %in% new_params, !(types[, names(D)[j]]	%in%	paste0(Ds[j], D[j][[1]]))] <- 0
  	}
		attr(P, "confounds") <- data.frame(t(mapply(c, names(confound[[1]]), names(confound[[2]]))))
		}

	colnames(P)  <- do.call(paste, c(types, sep =""))
	rownames(P ) <- pars
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


