#' types_to_rows
#'
#' @param model A model created by make_model()
#' @param query a query
#' @export
types_to_rows <- function(model, query){
	P <- get_parameter_matrix(model)
	model$P <- P
	param_set <- attr(P,"param_set")
  # 	If no confounds in model use nodal types to get at parameters
		if(is.null(attr(P, "confounds"))){
			return_rows <- types_to_nodes(model, query)
		} else{

	if (length(query) == 1){
		return_rows <- gbiqq:::types_to_rows_single(model, query)

	} else if (length(query) > 1){

		return_rows <- lapply(query, function(q) types_to_rows_single(model, q))
	  return_rows <- unlist(return_rows, recursive = FALSE)
		vars <- unique(attr(P,"param_set"))
		vars <- vars[vars %in% names(return_rows)]
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
#' @export
types_to_rows_single <- function(model, query){
	P <- model$P
	param_set <- attr(P,"param_set")

			nodal_types <- gbiqq:::types_to_nodes(model, query)
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

			} else{
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
query_to_parameters <- function(model, query){
	if(length(query) == 1){
		# For alpha containing only one query expression
		# 	1. get row in the parameter matrix that correspond to query
		# 	2. name output vector as paramater name
		#   3. assigns numeric value of alpha as specified in query

		translated_a  <-  gbiqq:::types_to_rows(model, names(query))
		translated_a  <- 	sapply(translated_a, function(a){
			a_temp <- query
			names(a_temp) <- a
			a_temp
		}, simplify = FALSE)

	} else{
		# For alpha containing multiple query expressions
		#   1. get rows in P that correspond to queries
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




