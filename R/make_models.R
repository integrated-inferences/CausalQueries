#' Make a model
#'
#' \code{make_model} uses dagitty syntax and functionality to specify nodes and edges of a graph. Implied causal types are calculated and default priors are provided under the assumption of no confounding.
#' Models can be updated with specification of a parameter matrix, P, by providing restrictions on causal types, and/or by providing informative priors on parameters.
#'
#' @param statement A character vector of length 1L. Statement describing causal relations using dagitty syntax. Only directed relations are permitted. For instance "X -> Y" or  "X1 -> Y <- X2; X1 -> X2".
#' @param add_priors A logical scalar. If \code{TRUE} sets default priors for the model.
#' @param add_parameters A logical scalar. If \code{TRUE} sets "true" parameters to the model.
#' @export
#'
#' @return An object of class probabilistic_causal_model containing a DAG.
#' @examples
#' modelXKY <- make_model("X -> K -> Y; X -> Y")
#'
#' # Example where cyclicaly dag attempted
#' \dontrun{modelXKX <- make_model("X -> K -> X")}


make_model <- function(statement, add_priors = TRUE, add_parameters = TRUE){

	dag <- dagitty::edges(dagitty::dagitty(	paste0("dag{", statement, "}")))
	if(!all(dag$e == "->")) stop("Please provide directed edges only")
	dag  <- dag[,1:2]
	names(dag) <- c("parent", "children")
	dag


	# Procedure to order dag
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


	# Model is a list
	model <- list(dag = dag[order(gen, dag[,1], dag[,2]),],
								step = "dag" )

	# Add a unique ordering of variables such that no earlier variables are caused by later variables
	endog_node <- as.character(rev(unique(rev(model$dag$children))))
	.exog_node <- as.character(rev(unique(rev(model$dag$parent))))
	exog_node  <- .exog_node[!(.exog_node %in% endog_node)]
	model$variables  <- c(exog_node, endog_node)
	attr(model, "endogenous_variables") <- endog_node
	attr(model, "exogenous_variables")  <- exog_node

	if(add_priors)     model <- set_priors(model)
	if(add_parameters) model <- set_parameters(model, type = "flat")

	class(model) <- "causal_model"

	return(model)
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
		sapply(model$variables, function(node){
			cat(paste0(node, ": ", length(restrictions[[node]]), " restricted types \n")  )
		})

	}







}
