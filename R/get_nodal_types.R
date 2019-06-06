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


#' Get list of types for variables in a DAG
#'
#' As type labels are hard to interpret for large models, the type list includes an attribute to help interpret them. See  \code{attr(types, interpret)}
#'
#' @param model A model created by make_model()
#' @importFrom rlang is_empty
#' @export
#'
#' @return A list of parents in a DAG
#'
#' @examples
#' require("dplyr")
#' model <- make_model("X -> K -> Y")
#' get_nodal_types(model)
#'
#' model <- make_model("X -> K -> Y") %>%
#'    set_restrictions(causal_type_restrict= "K[X=1]>K[X=0]") %>%
#'    set_confound(list(K = "Y[K=1]>Y[K=0]"))
#' unlist(get_nodal_types(model))
get_nodal_types <- function(model, query = NULL, collapse = TRUE){

	if(!is.null(query)){
		nodal_types <- types_to_nodes(model, query)
		if(is_empty(nodal_types))
			stop(paste("There are no nodal types that satisfy the query condition \n", query))

	} else{
		nodal_types <- get_nodal_types_model(model, collapse)
	}

	return(nodal_types)

}

#' Get nodal types of model
#'
get_nodal_types_model <- function(model, collapse = TRUE) {

	#	if(!is.null(model$nodal_types)) return(nodal_types) ## Placeholder -- check why this was previously not here

	nodal_types <- model$nodal_types
	variables   <- model$variables
	parents     <- get_parents(model)
	dag         <- model$dag

	types       <- lapply(
		X = lapply(parents, length),
		FUN = function(parent_n){

			type_mat <- perm(2^parent_n)
			if(parent_n == 0){
				labels <- NULL
			} else {
				input_mat <- perm(parent_n)
				labels <- apply(input_mat,1,paste,collapse = "")
			}
			colnames(type_mat) <- labels

			return(type_mat)
		})

	types_interpret       <-
		lapply(parents,
					 FUN = function(parent){
					 	parent_n <- length(parent)
					 	if(parent_n == 0){
					 		labels <- "exogeneous"
					 	} else {
					 		input_mat <- perm(rep(2,parent_n))
					 		labels <-    apply(input_mat,1,function(j) paste0(parent, " <- ", j ,collapse = " & "))
					 	}
					 	labels
					 })

	types_labels <- lapply(1:length(types), function(i){
		var <- names(types)[i]
		mat <- types[[i]]
		labels <- apply(mat,1,paste,collapse = "")
		paste0(var, labels)
	})

	names(types_labels )<- var_names <- names(types)
	types <- lapply(variables, function(v){
		rownames(types[[v]]) <- types_labels[[v]]
		types[[v]]
	})

	names(types)  <- var_names

	if(!is.null(nodal_types)){
		types <- lapply(variables, function(v){
			mat <- types[[v]]
			cn  <- colnames(mat)
			nt  <- nodal_types[[v]]
			mat <- mat[nt, ]
			colnames(mat) <- cn
			mat
		})
	}
	names(types)  <- var_names
	if(collapse){

		types <-	sapply(1:length(types), function(i){
			var <- names(types)[i]
			mat <- as.matrix(types[[i]])
			labels <- apply(mat,1,paste,collapse = "")
			paste0(var, labels)
		})

	}

	names(types)  <- var_names

	attr(types, "interpret") <- types_interpret
	return(types)
}

