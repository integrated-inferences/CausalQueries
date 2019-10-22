#' Make a model
#'
#' \code{make_model} uses dagitty syntax and functionality to specify nodes and edges of a graph. Implied causal types are calculated and default priors are provided under the assumption of no confounding.
#' Models can be updated with specification of a parameter matrix, P, by providing restrictions on causal types, and/or by providing informative priors on parameters.
#' The default setting for a causal model have flat (uniform) priors and parameters putting equal weight on each parameter within each parameter set. These can be adjust with \code{set_priors} and \code{set_parameters}
#'
#' @param statement A character vector of length 1L. Statement describing causal relations using dagitty syntax. Only directed relations are permitted. For instance "X -> Y" or  "X1 -> Y <- X2; X1 -> X2".
#' @export
#'
#' @return An object of class probabilistic_causal_model containing a DAG.
#' @examples
#' modelXKY <- make_model("X -> K -> Y; X -> Y")
#'
#' # Example where cyclicaly dag attempted
#' \dontrun{modelXKX <- make_model("X -> K -> X")}

make_model <- function(statement){

	dag <- dagitty::edges(dagitty::dagitty(	paste0("dag{", statement, "}")))
	if(!all(dag$e == "->")) stop("Please provide directed edges only")
	dag  <- dag[,1:2]
	names(dag) <- c("parent", "children")

	# Procedure for unique ordering of variables

	if(all(dag$parent %in% dag$children)) stop("No root nodes provided")

	gen <- rep(NA, nrow(dag))
	j = 1
	# assign 1 to exogenous variables
	gen[!(dag$parent %in% dag$children)] <- j
	while(sum(is.na(gen))>0) {
		j <- j+1
		x <- (dag$parent %in% dag$children[is.na(gen)])
		if(all(x[is.na(gen)])) stop(paste("Cycling at generation ", j))
  	gen[!x & is.na(gen)] <- j
  	}

  dag <- dag[order(gen, dag[,1], dag[,2]),]

 endog_node <- as.character(rev(unique(rev(dag$children))))
 .exog_node <- as.character(rev(unique(rev(dag$parent))))
 exog_node  <- .exog_node[!(.exog_node %in% endog_node)]


 # Model is a list
 model <- list(dag = dag, step = "dag", variables = c(exog_node, endog_node))

 # Nodal types
 nodal_types <- get_nodal_types(model)
 model$nodal_types <- nodal_types

 # Parameters dataframe
 m  <- length(nodal_types)
 model$parameters_df <- data.frame(

 	param_family = unlist(sapply(1:m, function(j) rep(names(nodal_types)[j], length(nodal_types[[j]])))),
 	param_set    = unlist(sapply(1:m, function(j) rep(names(nodal_types)[j], length(nodal_types[[j]])))),
  param_names  = unlist(sapply(1:m, function(i) paste0(names(nodal_types[i]), ".", nodal_types[i][[1]]))),
  param        = unlist(sapply(1:m, function(i) nodal_types[i][[1]])),
  node         = unlist(sapply(1:m, function(i) nodal_types[i][[1]])),
  parameters   = unlist(sapply(1:m, function(j) rep(1/length(nodal_types[[j]]), length(nodal_types[[j]])))),
  priors       = 1,
  stringsAsFactors = FALSE

  )

 # Prep for export
 attr(model, "endogenous_variables") <- endog_node
 attr(model, "exogenous_variables")  <- exog_node
 class(model) <- "causal_model"

 model

}



#' @export
print.causal_model <- function(x, ...) {
	print(summary(x))
	invisible(x)
}


#' @export
summary.causal_model <- function(object, ...) {
	structure(object, class = c("summary.causal_model", "data.frame"))

}

#' @export
print.summary.causal_model <- function(x,  ...){

	cat("\nDAG: \n")
	print(x$dag)
	cat("\n ------------------------------------------------------------------------------------------\n")
	cat("\nNodal types: \n")

	nodal_types <- get_nodal_types(x)
		nodes <- x$variables
		sapply(nodes, function(n){
			nt <- nodal_types[[n]]
			interpret <- attr(nodal_types, "interpret")[[n]]
			stop_at <- min(length(nt), 16)
			cat(paste0("$", n,"\n"))

			cat(paste0(nt[1:stop_at], collapse = "  ") )

			if(stop_at != length(nt)) cat(paste0(length(nt) - 16, " nodal types omitted"))
			cat("\n\n")
			print(interpret)
			cat("\n")
		})
		cat("\nNumber of types by node\n")

		print(sapply(nodal_types , length, USE.NAMES = TRUE))

  if(!is.null(x$P)){
  	cat("\n ------------------------------------------------------------------------------------------\n")
  	cat("\nParameter matrix: \n")
  	P <- x$P
  	cat(paste0("Number of parameters (rows): ", nrow(P), "\n"))
  	cat(paste0("Number of unit types (columns): ", ncol(P), "\n"))
  	if(!is.null(attr(P, "confounds") )){
  	cat("\nConfounds: \n")
  	print(attr(P, "confounds") )
  	}

  } else{

  	cat("\nNumber of unit types:")
    cat(paste0("  ", nrow(get_causal_types(x)), "\n"))

  }

	if(!is.null(attr(x,"restrictions"))){

		restrictions <- attr(x,"restrictions")
		cat("\n ------------------------------------------------------------------------------------------\n")
		cat("\nRestrictions: \n")
		sapply(x$variables, function(node){
			cat(paste0(node, ": ", length(restrictions[[node]]), " restricted types \n")  )
		})

	}

}
