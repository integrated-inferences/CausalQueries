#' n_check
#'
#' @param n A sample size argument.
#'
#' @details Checks whether the input is an integer bigger than 0.
#'

n_check <- function(n){
	cond1 <- !(round(n) == n)
	cond2 <- n <= 0
	cond_joint <- cond1 | cond2
	if (cond_joint) stop("Number of observation has to be an integer bigger than 0.")
}
