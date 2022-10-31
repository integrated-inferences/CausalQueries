#' Realise outcomes
#'
#' Realise outcomes for all causal types. Calculated by sequentially calculating endogenous nodes.
#' If a do operator is applied to any node then it takes the given value and all its descendants are generated accordingly.
#'
#' @details \code{realise_outcomes} starts off by creating types (via \code{\link{get_nodal_types}}). It then takes types of endogenous and reveals their outcome based on the value that their parents took. Exogenous nodes outcomes correspond to their type.
#' @inheritParams CausalQueries_internal_inherit_params
#' @param dos A named \code{list}. Do actions defining node values, e.g., \code{list(X = 0, M = 1)}.
#' @param node A character. An optional quoted name of the node whose outcome should be revealed. If specified all values of parents need to be specified via \code{dos}.
#' @param add_rownames logical indicating whether to add causal types as rownmaes to the ouput
#' @return A \code{data.frame} object of revealed data for each node (columns) given causal / nodal type (rows) .
#' @export
#' @examples
#' \donttest{
#' model <- make_model("X -> Y")
#' realise_outcomes(model)
#'
#' model <- make_model("X1->Y;X2->M;M->Y")
#' realise_outcomes(model, dos = list(X1 = 1, M = 0))
#'
#' model <- make_model("X->M->Y")
#' realise_outcomes(model, dos = list(M = 1), node = "Y")
#'}
realise_outcomes <- function(model, dos = NULL, node = NULL, add_rownames = TRUE){

	# Housekeeping
	if(!is.null(node) & is.null(dos)){
	  stop("Do actions must be specified when node is not NULL")
	}

  if(length(model$nodes) == 1){

    data_realizations <- get_causal_types(model)

    if(add_rownames){
      rownames(data_realizations) <- gsub("[[:alpha:]]","",rownames(data_realizations))
    }

    return(data_realizations)
  }

  nodal_types <- get_nodal_types(model, collapse = FALSE)
  nodal_types_collapsed <- get_nodal_types(model, collapse = TRUE)
  parents_list  <- get_parents(model)

	# Case with node specified
	if(!is.null(node)){

		parents <- parents_list[[node]]

		if(any(!names(dos) %in% parents)) {
			conjugation <- ifelse (sum(!names(dos) %in% parents)>1, "are not parents of", "is not a parent of")
			subjects <- paste0(names(dos)[!names(dos) %in% parents], collapse = ", ")
			if(all(!names(dos) %in% parents)){
				stop(paste(subjects, conjugation, node))
			} else {
				warning(paste(subjects, conjugation, node))
			}
		}

		nodal_type_var <- nodal_types[[node]]
		dos_rep <- as.character(sapply(parents, function(p) rep(dos[p], nrow(nodal_type_var))))

		data_realizations <- data.frame(dos_rep, rownames(nodal_type_var), stringsAsFactors = FALSE)
		names(data_realizations) <- c(parents, node)

		endogenous_vars <- node
		in_dos <- names(dos)

		if(add_rownames){
		  types <- data_realizations
		  types_of_endogenous <- data.frame(types[, endogenous_vars], stringsAsFactors = FALSE)
		  names(types_of_endogenous) <- endogenous_vars
		}

	}

	#case without node specified
	if(is.null(node)) {

	  data_realizations <- get_causal_types(model)
	  endogenous_vars <- attr(model, "nonroot_nodes")
	  in_dos <- names(dos) # vars in do list

	  if(add_rownames){
	    types <- data_realizations
	  }

		# Fill in values for dos
		if(!is.null(dos)){
		  for(j in 1:length(dos)){
		    data_realizations[, in_dos[j]] <- as.character(dos[[j]])
		  }
		}

	}

	# Magic: Work though each endogenous node in sequence and substitute its implied values
	endogenous_vars <- endogenous_vars[!(endogenous_vars %in% in_dos)]

	data_realizations <- realise_outcome_c(d = data_realizations,
	                                       endogenous_nodes = endogenous_vars,
	                                       parents_list = parents_list,
	                                       nodal_types_collapsed = nodal_types_collapsed,
	                                       nodal_types = nodal_types)

	# add rownames
	if(add_rownames){
	  if(is.null(node)){
	    rownames(data_realizations) <- apply(types, 1, paste, collapse = ".")
	    type_names <- matrix(sapply(1:ncol(types), function(j) paste0(names(types)[j], types[,j])), ncol = ncol(types))
	    attr(data_realizations, "type_names") <- apply(type_names, 1, paste,  collapse = ".")
	  } else {
	    attr(data_realizations, "type_names") <-
	      rownames(data_realizations) <- apply(types_of_endogenous, 1, FUN = function(x) paste0(x))
	  }
	}


	return(data_realizations)

}







#' Reveal outcomes
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' This function was deprecated because the name causes clashes with DeclareDesign. Use realise_outcomes instead.
#' @keywords internal
#'
reveal_outcomes <- function(model, dos = NULL, node = NULL) {
  lifecycle::deprecate_warn(
  "0.0.3.2",
  "reveal_outcomes()",
  details = "This function was deprecated because the name causes clashes with DeclareDesign. Use realise_outcomes instead."
)
  warning("This function was deprecated because the name causes clashes with DeclareDesign. Use realise_outcomes instead.")
realise_outcomes(model = model, dos = dos, node = node)
}


