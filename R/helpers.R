
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
