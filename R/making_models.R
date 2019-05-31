#' Make a model
#'
#' `make_model` uses dagitty syntax and functionality to specify nodes and edges of a graph. Imlpied causal types are calculated and default priors are provided under the assumption of no confounding.
#' Models can be updated with specification of a parameter matrix, P, by providing restrictions on causal types, and/or by providing informative priors on parameters. `
#'
#' @param statement A character statement describing causal relations using dagitty syntax. Only directed relations are permitted. For instance "X -> Y" or  "X1 -> Y <- X2; X1 -> X2" or
#' @param add_priors = FALSE Add default priors
#' @export
#'
#' @return An object of class probabilistic_causal_model containing a dag
#' @examples
#' modelXKY <- make_model("X -> K -> Y; X -> Y")
#'
#' # Example where cyclicaly dag attempted
#' modelXKX <- make_model("X -> K -> X")


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

	class(model) <- "probabilistic_causal_model"

	return(model)
}



#' @export
print.probabilistic_causal_model <- function(x, ...) {
	print(summary(x))
	invisible(x)
}


#' @export
summary.probabilistic_causal_model <- function(object, ...) {
	structure(object, class = c("summary.compared_diagnoses", "data.frame"))

}

#' @export
print.probabilistic_causal_model <- function(x, ...){

	cat("\n DAG: \n")
	print(x$dag)
  if(!is.null(x$P)){
  	cat("\n\n Parameter matrix: \n ")
  	print(x$P)
  }
	if(!is.null(x$lambdas_priors)){
		cat("\n\n lambda priors: \n")
		print(x$priors)
	}


}
