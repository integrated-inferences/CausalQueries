#' Restrict a model
#'
#' Restrict causal types. If priors exist prior probabilities are redistributed over remaining types.
#'
#' @param model a model created by make_model()
#' @param node_restrict a list of character vectors specifying nodal types to be removed from the model. Use \code{get_nodal_types} to see syntax.
#' @param causal_type_restrict  a quoted expressions defining the restriction
#' @param join_by A string. The logical operator joining expanded types when \code{causal_type_restrict} contains wildcard (\code{.}). Can take values \code{"&"} (logical AND) or \code{"|"} (logical OR). When restriction contains wildcard (\code{.}) and \code{join_by} is not specified, it defaults to \code{"|"}, otherwise it defaults to \code{NULL}.
#' @param action A string. Either `remove` or `keep` to indicate whether to remove or keep only types specified by \code{causal_type_restrict} or \code{node_restrict}.
#' @export
#' @return A model with restrictions and nodal types saved as attributes.
#'
#' @examples
#' require(dplyr)
#'
#' # Restrict parameter space using nodal types
#' model <- make_model("X->Y") %>%
#' set_restrictions(node_restrict = list(X = "X0", Y = "Y00"))
#' get_parameter_matrix(model)
#' # Restrict to a single type
#' model <- make_model("X->Y") %>%
#' set_restrictions(node_restrict = list(X = "X0", Y = c("Y00", "Y01", "Y11")))
#' get_parameter_matrix(model)
#' # Restrict to a single type
#' model <- make_model("X->Y") %>%
#' set_restrictions(node_restrict = list(Y = "Y11"), action = "keep")
#' get_parameter_matrix(model)
#' # Restrictions can be  with wildcards
#' model <- make_model("X->Y") %>%
#' set_restrictions(node_restrict = list(Y = "Y?0"))
#' get_parameter_matrix(model)
#' # Running example: there are only four causal types
#' model <- make_model("S -> C -> Y <- R <- X; X -> C -> R") %>%
#' set_restrictions(node_restrict = list(C = "C1000", R = "R0001", Y = "Y0001"), action = "keep")
#' get_parameter_matrix(model)
#'
#' # Restrict parameter space using casual types
#' model <- make_model("X->Y") %>%
#' set_restrictions(causal_type_restrict = c("X == 0", "Y==0"))
#' get_parameter_matrix(model)
#' # Restrict to define a model with monotonicity
#' model <- make_model("X->Y") %>%
#' set_restrictions(causal_type_restrict = c("Y[X=1] < Y[X=0]"))
#' get_parameter_matrix(model)
#' # Restriction with a wildcard
#' model <- make_model("X->Y<-M") %>%
#' set_restrictions(causal_type_restrict = c("(Y[X=1, M=.] < Y[X=0, M=.])"), join_by = "&")
#' get_parameter_matrix(model)
#'
set_restrictions <- function(model,
														 node_restrict = NULL,
														 causal_type_restrict = NULL,
														 join_by = NULL,
														 action = "remove"){

	if(is.null(node_restrict) & is.null(causal_type_restrict)) {message("No restrictions provided"); return(model)}

	if(!is.null(node_restrict)){
    model <- restrict_nodal_types(model,
    															restriction = node_restrict,
    															action = action)
	}
	if(!is.null(causal_type_restrict)){
		model <- restrict_causal_types(model, causal_type_restrict, join_by = join_by, action = action)
	}
	return(model)
}






#' Reduce causal types
#' @param model a model created by make_model()
#' @param restriction a quoted expressions defining the restriction
#' @param join_by A string. The logical operator joining expanded restriction types when restriction contains wildcard. Can take values \code{"&"} (logical AND) or \code{"|"} (logical OR). When restriction contains wildcard (\code{.}) and \code{join_by} is not specified, it defaults to \code{"|"}, otherwise it defaults to \code{NULL}.
#' @param action A string. Either `remove` or `keep` to indicate whether to remove or keep only causal types specified by \code{restriction}.
#' @export
restrict_causal_types <- function(model, restriction, join_by = NULL, action = "remove"){
	if(any(grepl(".", restriction, fixed = TRUE)) && is.null(join_by)) join_by <- "|"

	causal_types <- get_causal_types(model)
	if(length(restriction) == 1L){
		restricted_causal_types <- get_types(model, query = restriction, join_by = join_by)
		if(action == "remove"){
			model$causal_types <- causal_types[!restricted_causal_types$types,]
		} else {
			model$causal_types <- causal_types[restricted_causal_types$types,]
		}
	} else {
		restricted_causal_types_mat <- sapply(1:length(restriction), function(i){
			out <-  get_types(model, query = restriction[i], join_by = join_by)
			out$types
			})
		if(action == "remove"){
			restricted_causal_types <- apply(restricted_causal_types_mat, 1, any)
			model$causal_types <- causal_types[!restricted_causal_types,]
		} else {
			model$causal_types <- causal_types[restricted_causal_types$types,]
		}
		restricted_causal_types <- apply(restricted_causal_types_mat, 1, all)
		model$causal_types <- causal_types[restricted_causal_types,]
	}

	rownames(model$causal_types) <- 1:nrow(model$causal_types)
	type_names <- as.data.frame(type_names,   stringsAsFactors = FALSE)
	colnames(type_names) <- colnames(model$causal_types)
	unrestricted_nodal_types <- lapply(type_names, unique)
	unrestricted_nodal_types <- lapply(	unrestricted_nodal_types, as.character)
	current_nodal_types <- get_nodal_types(model)
	model$nodal_types <- sapply(model$variables, function(v) intersect(current_nodal_types[[v]], 	unrestricted_nodal_types[[v]] ), simplify = FALSE)

		# Subset priors and update
	type_names          <- get_type_names(model$nodal_types)
	if(!is.null(model$priors)) model$priors <- model$priors[type_names]
	if(!is.null(model$parameters)) model$parameters <- model$parameters[type_names]

	# model$causal_types  <- update_causal_types(model)

	return(model)

}

#' Reduce nodal types
#' @param model a model created by make_model()
#' @param restriction a list of character vectors specifying nodal types to be removed from the model. Use \code{get_nodal_types} to see syntax.
#' @param action A string. Either `remove` or `keep` to indicate whether to remove or keep only nodal types specified by \code{restriction}.
#' @export
restrict_nodal_types <- function(model, restriction, action = "remove"){

	variables   <- model$variables
	nodal_types <- get_nodal_types(model)

	# Stop if none of the names of the restrictions vector matches variables in dag
	# Stop if there's any restriction name that doesn't match any of the variables in the dag
	restricted_vars <- names(restriction)
	matches    <- restricted_vars %in% variables
	if(!any(matches)){
		stop("Restrictions don't match variables in DAG")
	} else if(any(!matches)){
		stop("Variables ", paste(names(restriction[!matches ]) ,"are not part of the model."))
	}

	# If there are wild cards, spell them out
	restriction_list <- lapply(restriction, function(j) unique(unlist(sapply(j, gbiqq:::unpack_wildcard))))

	# If "keep" specified, reverse meaning of restricted types -- only stipulated types to be kept
	if(!(action %in% c("remove", "keep"))) stop("action should be either 'remove' to 'keep'")
	if(action == "keep") for(j in names(restriction)){

	restriction_list <-
		sapply(names(restriction_list), function(j){
  	  nodal_types[[j]][!(nodal_types[[j]] %in% restriction_list[[j]])]
		}, simplify = FALSE)

	}
	nodal_types_restrictions <- nodal_types[restricted_vars]


	# Paste vars to restrictions when needed
	# For flexibility, restrictions can be written as "0" or "X0" for an exogenous var X
	# Though, we'd write its nodal_types as "X0" or "X1"
	# length(restrictions) = n_vars for which restrictions were specified
	restrictions_out <- lapply(1:length(restriction_list), function(i){

		#Identify nodal_types, var name and actual restriction statement for the current restriction
		ntr <- nodal_types_restrictions[[i]]
		restricted_var <- restricted_vars[i]
		restrictions_i <- restriction_list[[i]]
		if(length(ntr) == length(restrictions_i)) {
			stop(paste0("nodal_types can't be entirely reduced. Revise restrictions for variable ", restricted_var))
		}
		# Run through vector of restrictions for current restricted variable
		restrictions_i <- sapply(1:length(restrictions_i), function(j){
			restriction_ij <- restrictions_i[j]
			# if restriction doesn't contain variable's name
			# it pastes restricted var to restriction
			restriction_ij <- ifelse(grepl(restricted_var, restriction_ij),
															 restriction_ij,
															 paste0(restricted_var, restriction_ij))
			#  stop if restriction doesn't conform to nodal_types syntax (i.e. it doesn't look like Y00)
			#			if(!restriction_ij  %in% ntr){stop(paste0("Restriction ",	restriction_ij, " not conformable to nodal_types"))}
			# and return restrictions if it does.
			restriction_ij})

		## Nodal types are reduced here!
		nodal_types[[restricted_var]] <<- ntr[!ntr %in% restrictions_i]
		restrictions_i
	})

	names(restrictions_out)   <- restricted_vars
	model$nodal_types         <- nodal_types

	#	model$restrictions <- restrictions_out

	# Subset priors
	type_names          <- gbiqq:::get_type_names(nodal_types)
	model$priors <- model$priors[type_names]
	model$causal_types  <- gbiqq:::update_causal_types(model)
	if(!is.null(model$parameters)) model$parameters <-
		reduce_parameters(model, model$parameters)
	return(model)
}

#' Update causal types based on nodal types
#' Do not export
#'
update_causal_types <- function(model){

		possible_types <-	get_nodal_types(model)
		variables      <- names(possible_types)

		# Remove var names from nodal types
		possible_types <- lapply(variables, function(v) gsub(v, "", possible_types[[v]]))
		names(possible_types) <- variables

		# Get types as the combination of nodal types/possible_data. for X->Y: X0Y00, X1Y00, X0Y10, X1Y10...
		return_df <- data.frame(expand.grid(possible_types, stringsAsFactors = FALSE))

	return(return_df)
}


#' Reduce parameters
#' If parameters is longer than nodel types, because of a model restriction, subset lambda and renormalize
#' @param model a model created by make_model()
#' @param parameters a parameter vector possibly longer than the parameter length expected by model
#' @export

reduce_parameters <- function(model, parameters = model$parameters){

	variables   <- model$variables
	nodal_types <- get_nodal_types(model)
	type_names  <- gbiqq:::get_type_names(nodal_types)
	parameters  <- parameters[type_names]

	parameters <-
		unlist(sapply(variables, function(v){
		i <- which(startsWith(names(parameters), paste0(v, ".")))
		parameters[i]/sum(parameters[i])}, USE.NAMES = FALSE))

	parameters
	 }


#' Get type names
#'
get_type_names <- function(nodal_types) {
	unlist(sapply(1:length(nodal_types), function(i){
		name <- names(nodal_types)[i]
		a    <- nodal_types[[i]]
		paste(name, a, sep =".")
	}))}


#' Unpack a wild card
#'
unpack_wildcard <- function(x) {
	splitstring <- strsplit(x, "")[[1]]
	n_wild <- sum(splitstring=="?")
	if(n_wild ==0) return(x)
	variations <- perm(rep(1, n_wild))
	apply(variations, 1, function(j)  {
		z <- splitstring
		z[z=="?"] <- j
		paste0(z, collapse = "")})}

