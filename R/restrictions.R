#' Restrict a model
#'
#' Restrict causal types. If priors exist prior probabilities are redistributed over remaining types.
#'
#' @param model a model created by make_model()
#' @param node_restrict a list of character vectors specifying nodal types to be removed from the model. Use \code{get_nodal_types} to see syntax.
#' @export
#' @return A model with restrictions and nodal types saved as attributes.
#'
#' @examples
#' # restrictions can be specified following nodal_types syntax
#' XYmodel <- make_model("X->Y") %>%
#'   set_restrictions(node_restrict = list(X = "X0", Y = "Y00"))
#'
#' # or alternatively variable name can be omitted from restriction
#' XYmodel <- make_model("X->Y") %>%
#'    set_restrictions(model = XYmodel, node_restrict = list(X = "0", Y = "00"))
#'
#'
#' # Restrictions can be  with wildcards
#' my_model <-  set_restrictions(model = XYmodel, node_restrict = list(Y = "?0"))
#' get_parameter_matrix(my_model)

set_restrictions <- function(model, node_restrict = NULL){

	if(is.null(node_restrict)) {message("No restrictions provided"); return(model)}

	variables   <- c(attr(model, "exogenous_variables"), attr(model, "endogenous_variables"))
	nodal_types <- get_nodal_types(model)

	# Stop if none of the names of the restrictions vector matches variables in dag
	# Stop if there's any restriction name that doesn't match any of the variables in the dag
	restricted_vars <- names(node_restrict)
	matches    <- restricted_vars %in% variables
	if(!any(matches)){
		stop("Restrictions don't match variables in DAG")
	} else if(any(!matches)){
		stop("Variables ", paste(names(node_restrict[!matches ]) ,"are not part of the DAG."))
	}

	# If there are wild cards, spell them out
	node_restrict <- lapply(node_restrict, function(j) unique(unlist(sapply(j, unpack_wildcard))))


	nodal_types_restrictions <- nodal_types[restricted_vars]

	# Paste vars to restrictions when needed
	# For flexibility, restrictions can be written as "0" or "X0" for an exogenous var X
	# Though, we'd write its nodal_types as "X0" or "X1"
	# length(restrictions) = n_vars for which restrictions were specified
	restrictions_out <- lapply(1:length(node_restrict), function(i){

		#Identify nodal_types, var name and actual restriction statement for the current restriction
	  ntr <- nodal_types_restrictions[[i]]
		restricted_var <- restricted_vars[i]
		restrictions_i <- node_restrict[[i]]
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

  # Subset lambda_priors
	type_names          <- get_type_names(nodal_types)
	model$lambda_priors <- model$lambda_priors[type_names]

	return(model)
}




#' Reduce lambda
#' If lambda is longer than nodel types, because of a model restriction, subset lambda and renormalize
#' @param model a model created by make_model()
#' @param lambda a parameter vector possibly longer than the parameter length expected by model
#' @export

reduce_lambda <- function(model, lambda){

	variables <-  	c(attr(model, "exogenous_variables"),
									  attr(model, "endogenous_variables"))
	nodal_types <- get_nodal_types(model)
	type_names  <- get_type_names(nodal_types)
	lambda      <- lambda[type_names]

	unlist(sapply(variables, function(v){
		i <- which(startsWith(names(lambda), paste0(v, ".")))
		lambda[i]/sum(lambda[i])}))
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
	variations <- perm(rep(2, n_wild))
	apply(variations, 1, function(j)  {
		z <- splitstring
		z[z=="?"] <- j
		paste0(z, collapse = "")})}

