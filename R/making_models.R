#' Add edges to a DAG
#'
#' @param parent Character. The name of the parent variable.
#' @param children Character scalar or vector. The name(s) of the child(ren) variable(s).
#'
#' @export
#'
#' @return Edge(s) in a DAG
#'
#' @examples
#' \dontrun{
#'dag1 <- make_dag(
#' add_edges(parent = "X",children = c("K","Y")),
#' add_edges(parent = "K",children = "Y")
#' )
#' }
add_edges <- function(parent,children){
	data.frame(parent = parent, children = children)
}


#' Make a model
#'
#' `make_model` requires specifying nodes and edges of a graph. Imlpied causal types are calculated and default priors are provided under the assumption of no confounding.
#' Models can be updated with specification of a parameter matrix, P, by providing restrictions on causal types, and/or by providing informative priors on parameters. `
#'
#' @param ... Edges added by add_edges()
#'
#' @export
#'
#' @return An object of class probabilistic_causal_model containing a dag
#' @examples
#' \dontrun{
#' modelXKY <- make_model(
#' add_edges(parent = "X",children = c("K","Y")),
#' add_edges(parent = "K",children = "Y")
#' )
#'
#' # Example where variables need re-ordering
#' model <- make_model(
#'   add_edges(parent = "X3",  children = "Y"),
#'   add_edges(parent = "X2", children = "X3"),
#'   add_edges(parent = "X2", children = "Y"),
#'   add_edges(parent = "X1", children = "X2"))
#'
#' # Example where cyclicaly dag attempted
#' model <- make_model(
#'   add_edges(parent = "X1",  children = "X2"),
#'   add_edges(parent = "X2", children = "X3"),
#'   add_edges(parent = "X3", children = "X2"))

#' }

# dag <- data.frame(parent = c("X3","X2","X1"), children = c("Y", "X3", "X2"))

make_model <- function(...){
	dag <- rbind(...)

	# Procedure to order dag
	if(all(dag$parent %in% dag$children)) stop("No root nodes provided")

	gen <- rep(NA, nrow(dag))
	j = 1
	gen[!(dag$parent %in% dag$children)] <- j
	while(sum(is.na(gen))>0) {
		j <- j+1
		x <- (dag$parent %in% dag$children[is.na(gen)])
		if(all(x[is.na(gen)])) stop(paste("Cycling at generation ", j))
  	gen[!x & is.na(gen)] <- j
  	}

	dag <- list(dag = dag[order(gen, dag[,1], dag[,2]),], step = "dag" )

	model <- set_priors(dag)

	class(model) <- "probabilistic_causal_model"

	return(model)
}

