#' types_to_rows
#'
#' @param model A model created by make_model()
#' @param query a query
#' @export
types_to_rows <- function(model, query){
	P <- model$P
	param_set <- attr(P,"param_set")

	if(is.null(P)){
		return_rows <- gbiqq:::types_to_nodes(model, query)
	} else{
		if(is.null(attr(P, "confounds"))){
			return_rows <- types_to_nodes(model, query)
		} else{

	if (length(query) == 1){
		return_rows <- gbiqq:::types_to_nodes_single(model, query)

	} else if (length(query) > 1){
    P <- model$P
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



	}

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

			} else{
				matching_types <- get_types(model, query)
				matching_types <- names(matching_types$types[	matching_types$types])
				sapply(matching_types, function(type){
				 rows <- 	rownames(P)[P[,type ] == 1]
				 rows[rowSums(P[rows,])	 == 1]
				})

			}




			return(out)
}









