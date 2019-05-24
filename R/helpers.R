#' Get string between two regular expression patterns
#'
#' Returns a substring enclosed by two regular expression patterns. By default returns the name of the arguments being indexed by squared brackets (\code{[]}) in a string containing an expression.
#'
#' @param x A character vector of length 1L.
#' @param left A regular expression to serve as look ahead.
#' @param right A regular expression to serve as a look behind.
#' @return A character vector.
#' @examples
#' a <- "(XX[Y=0] == 1) > (XX[Y=1] == 0)"
#' st_within(a)
#' b <- "(XXX[[Y=0]] == 1 + XXX[[Y=1]] == 0)"
#' st_within(b)

st_within <- function(x, left = "[[:punct:]]|\\b", right = "\\["){
	if(!is.character(x)) stop("`x` must be a string.")
	puncts <- gregexpr(left, x, perl = TRUE)
	stops <- gregexpr(right, x, perl = TRUE)[[1]]

	# only index the first "[" when there are consecutive ones
	consec_brackets <- diff(stops)
	if(any(consec_brackets == 1)){
		remov <- which(consec_brackets == 1) + 1
		stops <- stops[-remov]
	}

	# find the closest punctuation or space
	starts <- sapply(stops, function(s){
		dif <- s - puncts[[1]]
		dif <- dif[dif>0]
		# print(dif)
		puncts[[1]][which(dif==min(dif))]}
	)

	sapply(1:length(starts), function(i) substr(x, starts[i], stops[i]-1))
}


retrieve_operator <- function(a, b){
	as <- strsplit(a, "")[[1]]
	bs <- strsplit(b, "")[[1]]
	intersect(bs[!bs %in% as], c("|", "&"))
}


expand_wildcard <- function(restriction, join_by = "|"){
	# aritmetic_operators  <- c("+", "-", "/", "*", "^")
	# relational_operators <- c(">",">=", "<","<=", "==", "!=")
	# operators <- c(aritmetic_operators, relational_operators)
	rest_oper <- sapply(strsplit(restriction, "\\\n"), trimws)
	#need to split by logical if followed by escape (\n)
	rest <- sapply(strsplit(restriction, "(\\||&)\\\n"), trimws)
	rest <- setdiff(rest, "")
	to_expand <- grepl("\\.", rest)

	expanded_types <- sapply(1:length(rest), function(i){
		if(!to_expand[i])
			return(rest[i])
		else {
			exp_types <- strsplit(rest[i], ".", fixed = TRUE)[[1]]
			a <- gregexpr("\\w{1}(?=(=\\.){1})", rest[i], perl = TRUE)
			ma <- unlist(regmatches(rest[i], a))
			rep_n <- sapply(unique(ma), function(e) sum(ma == e))
			n_types <- length(unique(ma))
			grid <- replicate(n_types, expr(c(0,1)))
			type_values <- do.call(expand.grid, grid)

			if(any(rep_n) > 0){
				type_values_v <- t(apply(type_values, 1, rep, times = rep_n))
			}else{
				type_values_v <- type_values
			}
			to_collapse <- cbind(type_values_v, "")
			ret <- sapply(1:nrow(to_collapse), function(t){
				tc <- cbind(exp_types, to_collapse[t,])
				paste0(apply(tc, 1, paste0), collapse = "")
			})

			return(ret)
		}
	})

	expr_operators <- lapply(1:length(expanded_types), function(i){
		retrieve_operator(a = expanded_types[[i]][1],
											b = rest_oper[i])
	})

	if(!is.null(join_by)){
		if(!is.list(expanded_types)) expanded_types <- list(expanded_types[,,drop=TRUE])
		to_print_list <- lapply(expanded_types, function(a){
			paste0(a, collapse = paste0(" ", join_by, "\n"))
		})
		to_return_list <- lapply(expanded_types, function(a){
			paste0(a, collapse = paste0(" ", join_by))
		})
	}else{
		to_print_list <- expanded_types
		to_return_list <- expanded_types
	}

	if(all(lapply(expr_operators, length) == 0)){
		to_print <- unlist(to_print_list)
		to_return <- unlist(to_return_list)
	}else{
		to_print <- paste0(unlist(lapply(to_print_list, function(l) paste0("(", l, ")"))),
											 collapse = paste0(" ", expr_operators, "\n"))
		to_return <- paste0(unlist(lapply(to_return_list, function(l) paste0("(", l, ")"))),
												collapse = paste0(" ", expr_operators))
	}
	cat("Generated expanded expression:\n")
	if(length(to_print)>1) cat(to_print, sep = "\n")
	else cat(to_print)
	to_return
}

restriction <- "(M[I=1] < M[I=0]) &
  (D[M=1, I=., P=.] < D[M=0, I=., P=.]) &
  (D[P=1, I=., M=.] < D[P=0, I=., M=.]) &
  (D[I=1, M=., P=.] > D[I=0, M=., P=.])"

expand_wildcard <- expand_restriction(restriction)
