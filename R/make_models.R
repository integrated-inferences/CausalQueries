#' Make a model
#'
#' \code{make_model} uses \link{dagitty} syntax and functionality to specify nodes and edges of a
#' graph. Implied causal types are calculated and default priors are provided under the
#' assumption of no confounding.
#' Models can be updated with specification of a parameter matrix, \code{P}, by
#' providing restrictions on causal types, and/or by providing informative priors on parameters.
#' The default setting for a causal model have flat (uniform) priors and parameters
#' putting equal weight on each parameter within each parameter set. These can be
#' adjust with \code{set_priors} and \code{set_parameters}
#'
#' @param statement A character. Statement describing causal
#' relations using dagitty syntax. Only directed relations are permitted.
#' For instance "X -> Y" or  "X1 -> Y <- X2; X1 -> X2".
#' @param add_causal_types Logical. Whether to dreate and attach causal types to \code{model}. Defaults to `TRUE`.
#' @export
#'
#' @return An object of class \code{causal_model} containing a DAG.
#' @examples
#' make_model(statement = "X -> Y")
#' modelXKY <- make_model("X -> K -> Y; X -> Y")
#'
#' # Example where cyclicaly dag attempted
#' \dontrun{
#'  modelXKX <- make_model("X -> K -> X")
#' }
#'
#' # Examples with confounding
#' model <- make_model("X->Y; X <-> Y")
#' model$P
#' model <- make_model("Y2 <- X -> Y1; X <-> Y1; X <-> Y2")
#' model$P
#' model$confounds_df
#' dim(model$P)
#' model$P
#' model <- make_model("X1 -> Y <- X2; X1 <-> Y; X2 <-> Y")
#' dim(model$P)
#' model$parameters_df
#'
#' # A single node graph is also possible
#' model <- make_model("X")
#' plot(model)
#'
#' # Unconnected nodes cannot
#' \dontrun{
#'  model <- make_model("X <-> Y")
#'  plot(model)
#' }

make_model <- function(statement, add_causal_types = TRUE){

	if(length(statement) != 1) stop("The length of the character vector of the statement is unequal to 1. Please provide only 1 causal model.")

	if(!(is.character(statement))) stop("The model statement should be of type character.")

	x <- dagitty::edges(dagitty::dagitty(	paste0("dag{", statement, "}"))) %>%
		data.frame(stringsAsFactors = FALSE)

	if(nrow(x)==0){ dag <- data.frame(v = statement, w = NA)
	} else {
	dag  <- x %>%
		dplyr::filter(e=="->") %>%
		dplyr::select(v,w)
	}

	if(length(x)>0 && any(!(unlist(x[,1:2]) %in% unlist(dag))))
		stop("Graph should not contain isolates.")

	names(dag) <- c("parent", "children")

	# allowable names
	node_names <- unique(c(as.character(dag$parent), as.character(dag$children)))
#	if(any(grepl("[.]", node_names))) stop("No dots in varnames please; try underscore?")
	if(any(grepl("-", 	node_names))) stop("No hyphens in varnames please; try dots?")
	if(any(grepl("_", node_names))) stop("No underscores in varnames please; try dots?")

	# Procedure for unique ordering of nodes
	if(all(dag$parent %in% dag$children)) stop("No root nodes provided.")

	gen <- rep(NA, nrow(dag))
	j <- 1
	# assign 1 to exogenous nodes
	gen[!(dag$parent %in% dag$children)] <- j
	while(sum(is.na(gen))>0) {
		j <- j+1
		xx <- (dag$parent %in% dag$children[is.na(gen)])
		if(all(xx[is.na(gen)])) stop(paste("Cycling at generation", j))
  	gen[!xx & is.na(gen)] <- j
  }

  dag <- dag[order(gen, dag[,1], dag[,2]),]

 endog_node <- as.character(rev(unique(rev(dag$children))))
 if(all(is.na(endog_node))) endog_node <- NULL
 .exog_node <- as.character(rev(unique(rev(dag$parent))))
 exog_node  <- .exog_node[!(.exog_node %in% endog_node)]

 nodes <- c(exog_node, endog_node)

 # Model is a list
 model <- list(dag = dag, step = "dag", nodes = nodes, statement = statement)

 # Nodal types
 nodal_types <- get_nodal_types(model, collapse = TRUE)
 model$nodal_types <- nodal_types

 # Parameters dataframe
 m  <- length(nodal_types)
 lgths <- lapply(nodal_types, length) %>% unlist

 model$parameters_df <- data.frame(
 	param_names  = unlist(sapply(1:m, function(i) paste0(names(nodal_types[i]), ".", nodal_types[i][[1]]))),
 	param_value   = unlist(sapply(1:m, function(j) rep(1/length(nodal_types[[j]]), length(nodal_types[[j]])))),
 	param_set    = unlist(sapply(1:m, function(j) rep(names(nodal_types)[j], length(nodal_types[[j]])))),
 	node = unlist(sapply(1:m, function(j) rep(names(nodal_types)[j], length(nodal_types[[j]])))),
  # param        = unlist(sapply(1:m, function(i) nodal_types[i][[1]])),
  nodal_type = unlist(sapply(1:m, function(i) nodal_types[i][[1]])),
  gen = rep(1:m, lgths),
  priors       = 1,
  stringsAsFactors = FALSE

  )

 # Add causal types
 if(add_causal_types){
 model$causal_types <- update_causal_types(model)}

 # Add confounds if any provided
 # extract confounds df
 if(any(x$e=="<->")) {

	 	confounds <- NULL

	 	z  <- x %>% dplyr::filter(e=="<->") %>% dplyr::select(v,w)
	 	z$v <- as.character(z$v)
	 	z$w <- as.character(z$w)

	 	# Reorder by reverse causal order (thus in X -> Y we have type_Y conditional on type_X)
	 	for(i in 1:nrow(z)){
	 		z[i,] <- rev(nodes[nodes %in% sapply(z[i,], as.character)])
	 	}
	 	# Generate confounds list
	 	confounds <- as.list(as.character(z$w))
	 	names(confounds) <- z$v

	 	# Check on ineligible confound statements
	 	if(any(!(c(z$v, z$w) %in% nodes)))
	 	stop("Confound relations (<->) must be between nodes contained in the dag
	 				(i.e. that also have a direct relation (->).")

	 	model <- set_confound(model, confounds)

	  }



 # Prep for export
 attr(model, "endogenous_nodes") <- endog_node
 attr(model, "exogenous_nodes")  <- exog_node
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

	cat("\nStatement: \n")
	print(x$statement)
	cat("\nDAG: \n")
	print(x$dag)
	cat("\n ------------------------------------------------------------------------------------------\n")
	cat("\nNodal types: \n")

	nodal_types <- get_nodal_types(x)
		nodes <- x$nodes
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
  	}

  if(!is.null(x$causal_types)){
  		cat("\nNumber of unit types:")
  		cat(paste0("  ", nrow(get_causal_types(x)), "\n"))}

	# Confounds dataframe
	if(!is.null(x$confounds_df) ){
			cat("\nConfounds: \n")
			print(x$confounds_df)}

	# List of restrictions kept as an attribute of model

	if(!is.null(attr(x,"restrictions"))){

		restrictions <- attr(x,"restrictions")
		cat("\n ------------------------------------------------------------------------------------------\n")
		cat("\nRestrictions: \n")
		sapply(x$nodes, function(node){
			cat(paste0(node, ": ", length(restrictions[[node]]), " restricted types \n")  )
		})

	}

}
