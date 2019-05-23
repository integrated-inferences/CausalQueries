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


get_str_diff <- function(a, b){
	as <- strsplit(a, "")[[1]]
	bs <- strsplit(b, "")[[1]]
	setdiff(bs[!bs %in% as], ".")
}


expand_wildcard <- function(restriction, joiner = "|"){
	rest_oper <- sapply(strsplit(restriction, "\\\n"), trimws)
	rest <- sapply(strsplit(restriction, "\\\n|\\||&"), trimws)
	relational <- c(">",">=", "<","<=", "==", "!=")

	rest <- setdiff(rest, "")
	to_expand <- grepl("\\.", rest)
	# take_operator <- grep("", value = TRUE)

	expanded_types <- sapply(1:length(rest), function(i){
		if(!to_expand[i])
			return(rest[i])
		else {
			exp_types <- strsplit(rest[i], ".", fixed = TRUE)[[1]]
			n_types <- (length(exp_types) - 1)/2
			grid <- replicate(n_types, expr(c(0,1)))
			type_values <- do.call(expand.grid, grid)
			is_relational <- sum(sapply(relational, function(i)
				any(grepl(i, exp_types, fixed = TRUE))))
			if(is_relational > 0){
				type_values_v <- t(apply(type_values, 1, rep, times = is_relational+1))
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

	operators <- lapply(1:length(expanded_types), function(i){
		retrieve_operator(a = expanded_types[[i]][1],
											b = rest_oper[i])
	})

	these <- lapply(expanded_types, function(a){
		paste0(a, collapse = paste0(joiner, "\n"))
	})

	to_print <- paste0(unlist(lapply(these, function(l) paste0("(", l, ")"))),
										 collapse = paste0(operators, "\n"))

	# to_print <- paste0(unlist(expanded_types), collapse = paste0(joiner, "\n"))
	cat(paste0("Generated expanded restriction:\n", to_print))
	to_return <- paste0(unlist(lapply(expanded_types, function(a){
		paste0("(", a, ")", collapse = joiner)
	})), collapse = unlist(operators))
	to_return
}


restriction <- "(M[I=1] < M[I=0]) |
  (D[M=1, I=., P=.] < D[M=0, I=., P=.]) |
  (D[P=1, I=., M=.] < D[P=0, I=., M=.]) |
  (D[I=1, M=., P=.] > D[I=0, M=., P=.])"

expand_wildcard <- expand_restriction(restriction, joiner = "|")

restriction <- "(M[I=1] < M[I=0]) &
  (D[M=1, I=., P=.] < D[M=0, I=., P=.]) &
  (D[P=1, I=., M=.] < D[P=0, I=., M=.]) &
  (D[I=1, M=., P=.] > D[I=0, M=., P=.])"

expand_wildcard <- expand_restriction(restriction)
