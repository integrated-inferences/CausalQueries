#' Restrict a model
#'
#' @param dag a dag created by make_dag()
#' @param restrictions a list of character vectors. Names in list should match dag's variables. Restrictions are specificified by as nodal types.
#' @export
#' @return A dag with restrictions and nodal types saved as attributes.
#'
#' @examples
#' XYdag <- make_dag(add_edges(parent = "X", children = c("Y")))
#' # restrictions can be specified following nodal_types syntax
#' reduce_nodal_types(dag = XYdag, restrictions(X = "X0", Y = "Y00"))
#'
#' # or alternatively variable name can be omitted from restriction
#' reduce_nodal_types(dag = XYdag, restrictions(X = "0", Y = "00"))
reduce_nodal_types <- function(pcm, restrictions){


variables       <- get_variables(pcm)
nodal_types     <- get_nodal_types(pcm)
restricted_vars <- names(restrictions)
matches         <- restricted_vars %in% variables


# Stop if none of the names of the restrictions vector matches variables in dag
# Stop if there's any restriction name that doesn't match any of the variables in the dag
if(!any(matches)){
	stop("Restrictions don't match variables in DAG")
} else if(any(!matches)){
	stop("Variables ", paste(names(restrictions[!matches ]) ,"are not part of the DAG."))
}

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
		if(!restriction_ij  %in% ntr){
				stop(paste0("Restriction ",	restriction_ij, " not conformable to nodal_types"))
		}
		# and return restrictions if it does.
		restriction_ij})

  ## HERE nodal types are reduced!
	nodal_types[[restricted_var]] <<- ntr[!ntr %in% restrictions_i]
	restrictions_i
	})

names(restrictions_out)   <- restricted_vars
pcm$nodal_types  <- nodal_types
pcm$restrictions <- restrictions_out


# TO DO: define print.dag ?

return(pcm)
}
