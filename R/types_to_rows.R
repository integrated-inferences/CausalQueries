#' types_to_rows
#'
#' @param model A model created by make_model()
#' @param query a query
#' @keywords internal
types_to_rows <- function(model, query){

	P <- get_parameter_matrix(model)
	model$P <- P
	param_set <- attr(P,"param_set")

  # 	If no confounds in model use nodal types to get at parameters
		if(is.null(attr(P, "confounds"))){
			return_rows <- types_to_nodes(model, query)
		} else{


	if (length(query) == 1){
		return_rows <- types_to_rows_single(model, query)

		  # if multiple queries
	} else if (length(query) > 1){

		# 1. return rows that map to each of the queries
		return_rows <- lapply(query, function(q) types_to_rows_single(model, q))
	  return_rows <- unlist(return_rows, recursive = FALSE)

	  # 2. Identify variable in the parameter set that correspond to the rows found in lines above
		vars <- unique(attr(P,"param_set"))
		vars <- vars[vars %in% names(return_rows)]

		# Prepare named list for output.
		# First level names correspond to variables names in param set (step 2.)
		# Second level names are queries/statements
		# The values in second-level vectors represent rows in P that map to the query saved as name of the vector that contains them. if that makes sense
		# eg. for query = "(X == 1) & (Y[X=1] > Y[X=0])"
    #   return_rows
		# $X
		# $X$`X == 1`
		# [1] "X1"
		#
		# $Y
		# $Y$`(Y[X=1] > Y[X=0])`
		# [1] "Y01"


		return_rows <- sapply(vars, function(v){
			i <- which(names(return_rows) == v)
			out <- unlist(return_rows[i], recursive = FALSE)
			names(out ) <- sapply(names(out), function(x) {
				dots <- gregexpr("\\.", x, perl = TRUE)[[1]]
				start <- dots[length(dots)]
				substr(x, start + 1 , nchar(x))
			})

			out}, simplify = FALSE)
	}}





	return(return_rows)
}



#' types_to_rows_single
#'
#' @param model A model created by make_model()
#' @param query a query
#' @keywords internal
types_to_rows_single <- function(model, query){
	P <- model$P
	param_set <- attr(P,"param_set")

  # Note that types_to_rows is only called for confounded models
	    # If type_to_nodes returns a non_empty object (i.e if it actually finds nodal types that map to query)
	    # preps par_names so that values can be matched to nodal_types returned by query
			nodal_types <- types_to_nodes(model, query)
			if(length(nodal_types) > 0 ){
				par_names <- rownames(P)
			  names_temp <- sapply(par_names, function(x) {
			  	stop <- gregexpr("\\.", x, perl = TRUE)[[1]]
			  	stop <- stop[length(stop)]
			  	substr(x, 1 , stop -1)
			  })
			  names_temp[names_temp == ""] <- par_names[names_temp == ""]
			  out <- names(names_temp)[names_temp %in% unlist(nodal_types)]
        names(out) <- param_set[rownames(P) %in% out]
        out <- as.list(out)
        out <- lapply(out, function(o) {names(o) <- query; as.list(o)})

			} # if no  nodal types returned by types_to_node
			  # look for causal types that map to query, and subsequently determine
			  # whether there are rows in P that model exclusevely the given causal type.
			  # return those rows if any

			  else{
				matching_types <- get_types(model, query)
				matching_types <- names(matching_types$types[	matching_types$types])

				 rows <- P[,	matching_types ] == 1
				 if(!is.null(dim(rows))) {
				 rows <- apply(rows, 1, all)

				 } else{
				 	rows <- rows & (rowSums(P) ==1)
				 }
				out <- rownames(P)[rows]
				names(out) <- param_set[rownames(P) %in% out]

			}




			return(out)
}



#' Function to translate queries into parameter names
#'
#' @param model A model created by make_model()
#' @param query a query
#' @keywords internal
query_to_parameters <- function(model, query){
	if(length(query) == 1){
		# For alpha containing only one query expression
		# 	1. get the row(s) in the parameter matrix that map to query
		# 	2. name output vector as paramater names
		#   3. assigns numeric value of alpha as specified in query

		rows  <-  types_to_rows(model, names(query))
		translated_a  <- 	sapply(rows, function(r){

			r_temp <- rep(query, length(r))
			names(r_temp) <-r
			r_temp
		}, simplify = FALSE)
   attr(translated_a, "query") <-
   	sapply(rows, function(r){
   		out <- list(r)
   		names(out) <- names(query)
   		out
   	}, simplify = FALSE)

	} else 	if(length(query) > 1){
		# For alpha containing multiple query expressions
		#   1. get rows in P that map to queries
		#   2. sapply combines objects in list by paramater set
		#
		rows           <- types_to_rows(model, names(query))
		param_set      <- names(rows)
		translated_a <- sapply(param_set, function(var){
			v_rows <- rows[[var]]
			unlist(sapply(1:length(v_rows), function(j){
				.query               <- names(v_rows)[j]
				v_row               <- v_rows[[j]]
				value               <- query[.query] # assign value as specified in query
				translated_a        <- rep(value, length(v_row))
				names(translated_a) <- v_row
				translated_a}, simplify = FALSE))
		},
		simplify = FALSE)
		attr(translated_a, "query") <- 	rows
	}
	return(translated_a)
}




