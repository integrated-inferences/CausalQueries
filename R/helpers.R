
#' Produces the possible permutations of a set of variables
#'
#' @param v A vector of integers indicating the number of values each variable takes on. E.g., a binary variable is represented by 2.
#'
#' @export
#'
#' @return A matrix of permutations
#'
#' @examples
#'
#' \dontrun{
#' perm(c(2,2,2))
#' }
perm <- function(v) {
	sapply(1:length(v), function(x) {
		rep(rep(1:v[x], each = prod(v[x:length(v)])/v[x]), length.out = prod(v))
	}) - 1
}

#' Get string between two regular expression patterns
#'
#' Returns a substring enclosed by two regular expression patterns. By default returns the name of the arguments being indexed by squared brackets (\code{[]}) in a string containing an expression.
#'
#' @param x A character vector of length 1L.
#' @param left A regular expression to serve as look ahead.
#' @param right A regular expression to serve as a look behind.
#' @return A character vector.
 #' @export
#' @examples
#' a <- "(XX[Y=0] == 1) > (XX[Y=1] == 0)"
#' st_within(a)
#' b <- "(XXX[[Y=0]] == 1 + XXX[[Y=1]] == 0)"
#' st_within(b)

st_within <- function(x, left = "[[:punct:]]|\\b", right = "\\[", rm_left = 0, rm_right=-1){
	if(!is.character(x)) stop("`x` must be a string.")
	puncts <- gregexpr(left, x, perl = TRUE)[[1]]
	stops <- gregexpr(right, x, perl = TRUE)[[1]]

	# only index the first of the same boundary
	# when there are consecutive ones (eg. "[[")
	consec_brackets <- diff(stops)
	if(any(consec_brackets == 1)){
		remov <- which(consec_brackets == 1) + 1
		stops <- stops[-remov]
	}

	# find the closest punctuation or space
	starts <- sapply(stops, function(s){
		dif <- s - puncts
		dif <- dif[dif>0]
		ifelse(length(dif) == 0, ret <- NA, ret <- puncts[which(dif==min(dif))])
		return(ret)
	})
	drop <- is.na(starts) | is.na(stops)
	sapply(1:length(starts), function(i) if(!drop[i]) substr(x, starts[i]+rm_left, stops[i]+rm_right))
}

# Recursive substitution
gsub_many <- function(x, pattern_vector, replacement_vector, ...){
	if(!identical(length(pattern_vector), length(replacement_vector))) stop("pattern and replacement vectors must be the same length")
	for(i in seq_along(pattern_vector)){
		x <- gsub(pattern_vector[i], replacement_vector[i], x, ...)
	}
	x
}

expand_wildcard <- function(to_expand, join_by = "|"){
	orig <- st_within(to_expand, left= "\\(", right="\\)", rm_left = 1)
	outer <- gsub_many(to_expand, orig, paste0("%expand%", 1:length(orig)),
										 fixed = TRUE)
	is_expand <- grepl("\\.", orig)

	expanded_types <- sapply(1:length(orig), function(i){
		if(!is_expand[i])
			return(orig[i])
		else {
			exp_types <- strsplit(orig[i], ".", fixed = TRUE)[[1]]
			a <- gregexpr("\\w{1}(?=(=\\.){1})", orig[i], perl = TRUE)
			matcha <- unlist(regmatches(orig[i], a))
			rep_n <- sapply(unique(matcha), function(e) sum(matcha == e))
			n_types <- length(unique(matcha))
			grid <- replicate(n_types, expr(c(0,1)))
			type_values <- do.call(expand.grid, grid)
			colnames(type_values) <- unique(matcha)

			apply(type_values, 1, function(s){
				to_sub <- paste0(colnames(type_values), "(\\b)*=(\\b)*$")
				subbed <- gsub_many(exp_types, to_sub, paste0(colnames(type_values), "=", s), perl = TRUE)
				paste0(subbed, collapse = "")
			})
		}
	})

	if(!is.null(join_by)){
		oper <- sapply(expanded_types, function(l){
			paste0(l, collapse = paste0(" ", join_by, " "))
		})
		if(length(orig)==1 && length(orig)!=length(oper)){
			oper <- sapply(expanded_types, function(a) gsub("%expand%1", a, outer))
			oper_return <- paste0(oper, collapse = paste0(" ", join_by, " "))
		}else{
			oper_return <- gsub_many(outer,paste0("%expand%", 1:length(orig)), oper)
		}

	} else {
		oper <- do.call(cbind, list(expanded_types))
		oper_return <- apply(oper, 1, function(i) gsub_many(outer,
																												paste0("%expand%", 1:length(orig)),i))
	}
	cat("Generated expanded expression:\n")
	cat(unlist(oper_return), sep = "\n")
	oper_return
}
