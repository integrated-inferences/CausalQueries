#' Wrapper of types_to_nodes_single
#' Sapply multiple query conditions
#'
#' @param model A model created by make_model()
#' @param query a query
#' @export
#' @examples
#' types_to_nodes(make_model("X->Y"), "Y[X=1]==1")
#' types_to_nodes(make_model("X->Y"), "Y==1")
types_to_nodes <- function(model, query){

	if (length(query) == 1){
		return_nodal_types <- types_to_nodes_single(model, query)

	} else 	if (length(query) > 1){

		return_nodal_types <- sapply(query, function(q) types_to_nodes_single(model, q), USE.NAMES = FALSE)
		vars <- 	model$variables[model$variables %in% names(return_nodal_types)]
		return_nodal_types <- sapply(vars, function(v){
			i <- which(names(return_nodal_types) == v)
			out <- return_nodal_types[i]
			names(out) <- query[i]
			out}, simplify = FALSE)

	}

	return_nodal_types
}





#' Identify nodes that satisfy a causal type query
#'
#' @param model A model created by make_model()
#' @param query a query
#' @keywords internal
#' @importFrom  rlang is_empty
types_to_nodes_single <- function(model, query){

	causal_types <- get_causal_types(model)
	mapped_causal_types <- get_types(model, query = query)
	# get causal types that don't map to query
	unlinked_causal_types <- causal_types[!mapped_causal_types$types,]
	rownames(unlinked_causal_types) <- 1:nrow(unlinked_causal_types)

	# unlinked_nodal_types: the nodal types that when combined produce the unliked causal types
	type_names  <- sapply(1:ncol(causal_types), function(j) paste0(names(unlinked_causal_types)[j], unlinked_causal_types[,j]))
	unlinked_nodal_types <- lapply(as.data.frame(type_names, stringsAsFactors = FALSE), unique)
	variables <- names(unlinked_nodal_types) <- names(causal_types)

	model_nodal_types <- get_nodal_types(model)

	# get those nodal types that are in the model but not in the unlinked
	return_nodal_types <- sapply(variables, function(v) setdiff(model_nodal_types[[v]], 	unlinked_nodal_types[[v]] ))

  return_nodal_types[lengths(	return_nodal_types) > 0L]

}


#' types_to_rows
#'
#' @param model A model created by \code{make_model()}
#' @param query a query
#' @keywords internal
types_to_rows <- function(model, query){

	if(is.null(model$P)) 	model  <- set_parameter_matrix(model)
	P         <- model$P
	param_set <- model$parameters_df$param_set

	# 	If no confounds in model use nodal types to get at parameters
	if(is.null(attr(P, "confounds"))){

		return_rows <- types_to_nodes(model, query)

	} else {

		if (length(query) == 1){
			return_rows <- gbiqq:::types_to_rows_single(model, query)

			# if multiple queries
		} else if (length(query) > 1){

			# 1. return rows that map to each of the queries
			return_rows <- lapply(query, function(q) types_to_rows_single(model, q))
			return_rows <- unlist(return_rows, recursive = FALSE)

			# 2. Identify variable in the parameter set that correspond to the rows found in lines above
			vars <- unique(model$parameters_df$param_set)
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


	return_rows
}



#' types_to_rows_single
#'
#' @param model A model created by make_model()
#' @param query a query
#' @keywords internal
types_to_rows_single <- function(model, query){

	if(is.null(model$P)) 	{model  <- set_parameter_matrix(model)}
	P                  <- model$P
	param_set <-  model$parameters_df$param_set

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
	# whether there are rows in P that model the given causal type.
	# return those rows if any

	else{
		matching_types <- get_types(model, query)
		matching_types <- names(matching_types$types[	matching_types$types])

		rows <- P[,	matching_types ] == 1
		if(!is.null(dim(rows))) {
			rows <- apply(rows, 1, all)

		}
		# else{
		# 	rows <- rows & (rowSums(P) ==1)
		# }
		out <- rownames(P)[rows]
		names(out) <- param_set[rows]

	}

	return(out)
}



#' Function to translate queries into parameter names
#'
#' @param model A model created by make_model()
#' @param query a query
#' @keywords internal
#' @examples
#'
query_to_parameters <- function(model, query){

	if(is.null(model$P)) model <- set_parameter_matrix(model)
	P <- model$P

	if(length(query)== 1){
		# For alpha containing only one query expression
		# 	1. get the row(s) in the parameter matrix that map to query
		# 	2. name output vector as paramater names
		#   3. assigns numeric value of alpha as specified in query

		rows  <-  gbiqq:::types_to_rows(model, names(query))

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
 translated_a
}



#' Check for discrepancies
#' Used in make_alphas
#' @param alphas alphas provided by the user in set_priors/make_priors
#' @param translated_alphas alphas that were expressed as a causal query.
any_discrepancies <- function(alphas, translated_alphas){
	error_message <- NULL
	repeated_parameters <- names(unlist( 	translated_alphas  )) %in% names(unlist(alphas))
	if(any(repeated_parameters)){
		query_alpha <- attr(translated_alphas, "query")
		i_repeated  <-	which(repeated_parameters)
		q_repeated  <- unlist(translated_alphas)[i_repeated]
		names(q_repeated) <- names(unlist(translated_alphas))[i_repeated]
		names(q_repeated) <- sapply(  names(q_repeated) , function(q){
			stop <- gregexpr("\\.", q, perl = TRUE)[[1]][1]
			substr(q, stop + 1, nchar(q))
		})
		q_repeated <- q_repeated[order(names(q_repeated))]

		i_alphas_repeated <- names(unlist(alphas)) %in% names(unlist(translated_alphas))
		alphas_repeated <-  unlist(alphas)[i_alphas_repeated]
		names(alphas_repeated)  <-  names(unlist(alphas))[i_alphas_repeated]

		names(alphas_repeated) <- sapply(  names(alphas_repeated) , function(q){
			stop <- gregexpr("\\.", q, perl = TRUE)[[1]][1]
			substr(q, stop + 1, nchar(q))
		})
		alphas_repeated <-  alphas_repeated[order(names(alphas_repeated))]

		if(!identical(alphas_repeated, q_repeated)){
			a_discrepancies  <- 	alphas_repeated[alphas_repeated != q_repeated]
			q_discrepancies  <- 	q_repeated[alphas_repeated != q_repeated]
			adicrepancy_names <- names(alphas_repeated)[alphas_repeated != q_repeated]

			error_message <- unlist(sapply(query_alpha, function(q){
				sapply(1:length(q), function(j){
					r <- q[[j]] %in%  adicrepancy_names
					if(any(r)){
						paste0( names(q)[j] , " = ", q_discrepancies[ q[[j]][r]] ,", ", q[[j]][r], " = ", a_discrepancies[ q[[j]][r]], "\n")
					} })}))
		}
	}
	return(error_message)
}

