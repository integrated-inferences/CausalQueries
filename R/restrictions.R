#' Restrict a model
#'
#' Restrict causal types. If priors exist prior probabilities are redistributed over remaining types.
#'
#' @param model a model created by make_model()
#' @param restrictions a list of character vectors specifying nodal types to be removed from the model. Use \code{get_nodal_types} to see syntax.
#' @param do a list containing the names of variables and their assigned value.
#' @export
#' @return A model with restrictions and nodal types saved as attributes.
#'
#' @examples
#' XYmodel <- make_model(add_edges(parent = "X", children = c("Y")))
#' # restrictions can be specified following nodal_types syntax
#' set_restrictions(model = XYmodel, restrictions = list(X = "X0", Y = "Y00"))
#'
#' # or alternatively variable name can be omitted from restriction
#' set_restrictions(model = XYmodel, restrictions = list(X = "0", Y = "00"))
#'
#' # A particularly important restriction is the do operator that removes all but one nodal_type from a node
#' set_restrictions(model = XYmodel, dos = list(X = 1))
#' set_restrictions(model = XYmodel, dos = list(Y = 0))
#'
#' # Restrictions can be iteratively applied
#' my_model <-  set_restrictions(model = XYmodel, restrictions = list(Y = "10"))
#' my_model <-  set_restrictions(model = my_model, do = list(X = 1))
#' get_indicator_matrix(my_model)
#'
#' # Restrictions can be  with wildcards
#' my_model <-  set_restrictions(model = XYmodel, restrictions = list(Y = "?0"))
#' get_indicator_matrix(my_model)

set_restrictions <- function(model, restrictions = NULL, dos = NULL){

	if(is.null(restrictions)  & is.null(dos))  {message("No restrictions provided"); return(model)}
	if(!is.null(restrictions) & !is.null(dos)) stop("Provide either types to restrict or variables to fix")

	variables <-  	c(attr(model, "exogenous_variables"),
									  attr(model, "endogenous_variables"))
	nodal_types     <- get_nodal_types(model)

	# Stop if none of the names of the restrictions vector matches variables in dag
	# Stop if there's any restriction name that doesn't match any of the variables in the dag
	restricted_vars <- c(names(restrictions), names(dos))
	matches    <- restricted_vars %in% variables
	if(!any(matches)){
		stop("Restrictions don't match variables in DAG")
	} else if(any(!matches)){
		stop("Variables ", paste(names(restrictions[!matches ]) ,"are not part of the DAG."))
	}

	# If there are do vars then define restrictions as the complement of these
  # Redo with lapply?
	if(!is.null(dos)) {

		restrictions <- list()

		n_parents <- lapply(get_parents(model), length)

		for(i in 1:length(dos)) {
			v     <- names(dos)[i]
			types <- nodal_types[v][[1]]
			keep  <- paste0(v, paste0(rep(dos[v][[1]], 2^n_parents[v][[1]]), collapse = ""))
			restrictions[[i]] <- paste(types[!types%in%keep])
		}
		names(restrictions) <- names(dos)
	  }

	# If there are wild cards, spell them out
	restrictions <- lapply(restrictions, function(j) unique(unlist(sapply(j, unpack_wildcard))))

#	for(j in restrictions) restrictions_record

	nodal_types_restrictions <- nodal_types[restricted_vars]

	# Paste vars to restrictions when needed
	# For flexibility, restrictions can be written as "0" or "X0" for an exogenous var X
	# Though, we'd write its nodal_types as "X0" or "X1"
	# length(restrictions) = n_vars for which restrictions were specified
	restrictions_out <- lapply(1:length(restrictions), function(i){

		#Identify nodal_types, var name and actual restriction statement for the current restriction
	  ntr <- nodal_types_restrictions[[i]]
		restricted_var <- restricted_vars[i]
		restrictions_i <- restrictions[[i]]
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
#			if(!restriction_ij  %in% ntr){
#					stop(paste0("Restriction ",	restriction_ij, " not conformable to nodal_types"))
#			}
			# and return restrictions if it does.
			restriction_ij})

	  ## HERE nodal types are reduced!
		nodal_types[[restricted_var]] <<- ntr[!ntr %in% restrictions_i]
		restrictions_i
		})

	names(restrictions_out)   <- restricted_vars
	model$nodal_types  <- nodal_types
#	model$restrictions <- restrictions_out

  # Subset lambda_priors
	type_names  <- get_type_names(nodal_types)
	model$lambda_priors <- model$lambda_priors[type_names]


	# TO DO: define print.dag ?
	# Have restrictions for a class eg ("Z = monotonic in W")
  # CHeck adding old and new restrictions into restrictions attribute

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
		a   <- nodal_types[[i]]
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

