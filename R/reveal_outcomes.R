#' Reveal outcomes
#'
#' Reveal outcomes for all causal types. Calculated by sequentially calculating endogenous nodes.
#' If a do operator is applied to any node then it takes the given value and all its descendants are generated accordingly.
#'
#' @details \code{reveal_outcomes} starts off by creating types (via \code{\link{map_query_to_causal_type}}). It then takes types of endogenous and reveals their outcome based on the value that their parents took. Exogenous nodes outcomes correspond to their type.
#' @inheritParams gbiqq_internal_inherit_params
#' @param dos A named \code{list}. Do actions defining node values, e.g., \code{list(X = 0, M = 1)}.
#' @param node A character. An optional quoted name of the node whose outcome should be revealed. If specified all values of parents need to be specified via \code{dos}.
#' @return A \code{data.frame} object of revealed data for each nodel (columns) given causal / nodal type (rows) .
#' @export
#' @examples
#' model <- make_model("X -> Y")
#' reveal_outcomes(model)
#'
#' model <- make_model("X1->Y;X2->M;M->Y")
#' reveal_outcomes(model, dos = list(X1 = 1, M = 0))
#'
#' model <- make_model("X->M->Y")
#' reveal_outcomes(model, dos = list(M = 1), node = "Y")
#'
reveal_outcomes <- function(model, dos = NULL, node = NULL){

	# Housekeeping
	if(!is.null(node) & is.null(dos)) stop("Do actions must be specified when node is not NULL")

	nodal_types <- get_nodal_types(model, collapse = FALSE)
	parents_list  <- get_parents(model)

	# Case with node specified
	if(!is.null(node)){

		parents <- parents_list[[node]]

			if(any(!names(dos) %in% parents)) {
				conjugation <- ifelse (sum(!names(dos) %in% parents)>1, "are not parents of", "is not a parent of")
				subjects <- paste0(names(dos)[!names(dos) %in% parents], collapse = ", ")
				if(all(!names(dos) %in% parents))
					stop(paste(subjects, conjugation, node))
				else
					warning(paste(subjects, conjugation, node))
			}

			nodal_type_var <- nodal_types[[node]]
			nodal_label    <- apply(nodal_type_var, 1,paste, collapse ="")

			dos_rep        <- sapply(parents, function(p) rep(dos[p], length(nodal_label)))

			data_realizations <- data.frame(dos_rep, nodal_label, stringsAsFactors = FALSE)
			names(data_realizations) <- c(parents, node)
			types <- data_realizations

			endogenous_vars   <- 	node
			types_of_endogenous        <- data.frame(types[, endogenous_vars], stringsAsFactors = FALSE)
			names(types_of_endogenous) <- endogenous_vars
			in_dos <- names(dos) # vars in do list
		}

	if(is.null(node)) {

		types           <- get_causal_types(model)
		exogenous_vars  <- attr(model, "exogenous_nodes")
		endogenous_vars <- attr(model, "endogenous_nodes")
		types_of_exogenous         <- data.frame(types[, exogenous_vars])
		names(types_of_exogenous)  <- exogenous_vars
		types_of_endogenous        <- data.frame(types[, endogenous_vars], stringsAsFactors = FALSE)
		names(types_of_endogenous) <- endogenous_vars
		data_realizations <- types
		in_dos <- names(dos) # vars in do list

		# Fill in values for dos
		if(!is.null(dos)) for(j in 1:length(dos)) data_realizations[, in_dos[j]] <- dos[[j]] # [1] mh 20191008 Commented out this "[1]" as it was stopping filling in a vector of dos for nested expression

		}
  if(ncol(types_of_endogenous) > 0){
	# Magic: Work though each endogeneous node in sequence and substitute its implied values
	for(j in 1:ncol(types_of_endogenous)) {
		if(!(endogenous_vars[j] %in% in_dos)){   # skip if do applied to var

			var            <- endogenous_vars[j]
			child_type     <- types_of_endogenous[,j]
			parents        <- parents_list[[var]]
			nodal_type_var <- nodal_types[[var]]
			nodal_label    <- apply(nodal_type_var, 1,paste, collapse ="")

			J <- sapply(1:length(child_type), function(i){
				type        <- child_type[i]
				parents_val <- data_realizations[i, parents]
				parents_val <- trimws(paste(parents_val, collapse = ""))
				row         <- which(nodal_label == type )
				outcome     <- nodal_type_var[row, parents_val]
				outcome
			})
			data_realizations[, endogenous_vars[j]] <- J
		}
	}
  }

	# Prep for export
	if(is.null(node)){
		rownames(data_realizations) <- apply(types, 1, paste, collapse = ".")
		type_names <- matrix(sapply(1:ncol(types), function(j) paste0(names(types)[j], types[,j])), ncol = ncol(types))
		attr(data_realizations, "type_names") <- apply(type_names, 1, paste,  collapse = ".")
	} else {
		attr(data_realizations, "type_names") <-
			rownames(data_realizations) <- apply(types_of_endogenous, 1, FUN = function(x)paste0(# node,  ## Not uncomment to include node name
				x))
	}

	data_realizations

}




