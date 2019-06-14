types_to_rows <- function(model, query){

	if (length(query) == 1){
		return_rows <- types_to_nodes_single(model, query)

	} else 	if (length(query) > 1){

		return_rows <- sapply(query, function(q) types_to_rows_single(model, q), USE.NAMES = FALSE)
		vars <- 	model$variables[model$variables %in% names(return_rows)]
		return_rows <- sapply(vars, function(v){
			i <- which(names(return_rows) == v)
			out <- return_rows[i]
			names(out) <- query[i]
			out}, simplify = FALSE)
	}


	return(return_rows)
}






#' Identify nodes that satisfy a causal type query
#'
#' @importFrom  rlang is_empty
types_to_rows_single <- function(model, query){
	P <- model$P
	if(is.null(P)){
		out <- types_to_nodes(model, query)
	} else{
		if(is.null(attr(P, "confounds"))){
	  	out <- types_to_nodes(model, query)
		} else{

			nodal_types <- gbiqq:::types_to_nodes(model, query)
			if(length(nodal_types) > 0 ){
				par_names <- rownames(P)
			  names_temp <- sapply(par_names, function(x) {
			  	stop <- gregexpr("\\.", x, perl = TRUE)[[1]]
			  	stop <- stop[length(stop)]
			  	substr(x, 1 , stop -1)
			  })
			  names_temp[names_temp == ""] <- par_names[names_temp == ""]
			  out <- names_temp[names_temp %in% nodal_types]

			} else{
				matching_types <- get_types(model, query)
				matching_types <- names(matching_types$types[	matching_types$types])
				sapply(matching_types, function(type){
				 rows <- 	rownames(P)[P[,type ] == 1]
				 rows[rowSums(P[rows,])	 == 1]
				})

			}





		}

	}

}





