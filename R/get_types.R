

#' Get values of types according to a query
#'
#' @param model A model  created by \code{make_model}
#'
#' @param query A quoted expression to interrogate \code{reveal_outcomes}
#'
#' @export
#'
#' @return A list containing the types and the evaluated expression
#'
#' @examples
#' model <- make_model("X"%->%"M", "X"%->%"Y", "M"%->%"Y")
#' query <- "(Y[X=1] > Y[X=0]) & (M[X=0]==1)"
#' x <- get_types(model, query)
#' x$types[x$types]
#' query <- "Y[M=M[X=0], X=1]==1"
#' x <- get_types(model, query)
#' x$types[x$types]

get_types <- function(model, query){

	# Global Variables
	eval_var  <- list()
	k <- 1
	i <- 0
	list_names <- ""
	continue <- TRUE

	# strip whitespaces
	# split query into single characters
	# locate opening brackets and reverse oder
	w_query <- gsub(" ", "", query)
	w_query <- unlist(strsplit(query, ""))
	bracket_starts <- rev(grep( "\\[", w_query))
	bracket_ends    <- rev(grep( "\\]", w_query))

	if(length(bracket_starts) != length(bracket_ends)){
		stop("Either '[' or ']' missing")
	}
	if(length(bracket_starts) == 0){
		continue = FALSE
		eval_var <-  reveal_outcomes(model)
		list_names <- colnames(eval_var)
	}

	while(continue){
		i <- i + 1

		# start at the latest found "["
		# find the closest subsequent "]"
		# remove brackets and extract expression
		.query	<- w_query[(bracket_starts[i] ):length(w_query)]
		.bracket_ends <- grep("\\]", .query)[1]
		.query <- .query[1:.bracket_ends]
		brackets <- grepl("\\[|\\]",  .query)
		.query <- .query[!brackets]

		# Split expression by ","
		# "x = 1, m = 0" into
		# "x = 1" "m=0"
		.query <- paste0(.query, collapse = "")
		.query <- unlist(strsplit(.query, ","))
		dos <- list()

		# Walks through splitted expressions (i.e dos)
		# and evaulates each expression when possible
		for (j in 1:length(.query)) {
			do <- unlist(strsplit( .query[j], ""))
			value <- c(eval(parse(text = paste0(do, collapse = "") ), envir =  eval_var))
			vars  <-  model$variables
			v_cond  <- paste0(vars, collapse = "|")
			i_var <- grepl(v_cond, do) ## throw error if not var found ... also need to process comas
			var_name <- do[i_var]
			dos[[j]] <- value
			names(dos)[[j]] <- var_name
		}



		# Indentify corresponding variable
		# if it's the first [ in the query extract expression until that [
		# otherwise extract expression between square brackets
		# Find variable within that expression
    if(i == length(bracket_starts)){
    	b <- 1:bracket_starts[i]
    }else{
    	b <- bracket_starts[i] - bracket_ends
    	b[b<0] <- NA
    	b <- (bracket_ends[which.min(b)] + 1):bracket_starts[i]
    }
		var <- paste0(w_query[b], collapse = "")
		var <- st_within(var)

		# Save result from last iteration
		# and remove corresponding expression w_query
		var_length <- nchar(var)
		data <- reveal_outcomes(model, dos )
		eval_var[[k]] <-  as.numeric(data[, var])


		.bracket_ends <- bracket_starts[i] + .bracket_ends - 1
		s <- seq(bracket_starts[i] - var_length , .bracket_ends )
		list_names[k] <- paste0( w_query[s], collapse = "")
		names(list_names)[k] <- names(eval_var)[k] <- w_query[s[1]] <-  paste0("var",k)
		w_query[s[2:length(s)]] <- ""
		k <- k + 1

		# Stop loop there are no [] left
		if(!any(grep("\\[|\\]", w_query))){
			continue <- FALSE
    }
	}

	w_query <- paste0(w_query, collapse = "")
	w_query <- gsub(" ", "", w_query)
	types <- c(eval(parse(text = w_query),  eval_var))

	# p <- length(list_names)
	# for (i in 1:p) {
	# 	for(j in 1:p){
	# 		list_names[i] <- gsub(names(list_names)[j], list_names[j], list_names[i])
	# 	}
	# }

	names(eval_var) <- list_names
	if(is.null(model$P)){
		model$P <- get_parameter_matrix(model)
	}


	names(types) <- colnames(model$P)
  return_list <- 	list(types = types,
  										 query = query,
  										 exp   = eval_var)

	class(return_list) <- "causal_types"

	return(return_list)

}



#' @export
print.causal_types <- function(x, ...) {
	print(summary(x))
	invisible(x)
}


#' @export
summary.causal_types <- function(object, ...) {
	structure(object, class = c("summary.causal_types", "data.frame"))

}

#' @export
print.summary.causal_types <- function(x, ...){
  types1 <- x$types[x$types]
	cat(paste("\nCausal types satisfying query's condition(s)  \n\n query = ", x$query,  "\n\n"))

  if(length(types1) %% 2 != 0){
  	types1[length(types1) + 1] <- ""
  }
	counter <- 2
	while (counter <= length(types1) ) {
		cat(paste0(names(types1[(counter -1):counter]), collapse = "  "))
		cat("\n")
		counter <- counter + 2
	}


	cat(paste("\n\n Number of causal types that meet condition(s) = ", length(types1)))
	cat(paste("\n Total number of causal types in model = ", length(x$types)))

}


