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
