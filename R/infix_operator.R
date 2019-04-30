#' Add edge
#'
#' @param lhs parent, in quotes.
#' @param rhs child, in quotes
#'
#' @return df
#'#'
#' @importFrom rlang quos quo_expr eval_tidy quo_text lang_args is_formula is_symbol
#' @importFrom utils bibentry
#' @export
#'
#' @examples
#'
`%->%` <- function(lhs, rhs) {

	if (!inherits(rhs, "character") | !inherits(rhs, "character") ) {
		stop("Please provide strings to both sides of operator",
				 call. = FALSE)
	}

 gbiqq::add_edges(lhs, rhs)
}
