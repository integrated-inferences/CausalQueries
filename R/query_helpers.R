#' Make monotonicity statement (positive)
#'
#' Generate a statement for Y monotonic (increasing) in X
#'
#' @param X input variable
#' @param Y outcome variable
#' @family statements
#' @export
#'
#' @return A character statement of class statement
#' @examples
#' increasing("A", "B")
#'
increasing <- function(X, Y){

	if(!is.character(X) | !is.character(Y)) stop("Provide node names as strings in function: increasing")

	statement = paste0("(Y", "[", X, "=1] > ", Y, "[", X, "=0])")

	class(statement) <- "statement"

	statement
	}

#' Make monotonicity statement (negative)
#'
#' Generate a statement for Y monotonic (decreasing) in X
#'
#' @param X input variable
#' @param Y outcome variable
#' @family statements
#' @export
#' @return A character statement of class statement
#' @examples
#' decreasing("A", "B")
#'
decreasing <- function(X, Y){

	if(!is.character(X) | !is.character(Y)) stop("Provide node names as strings in function: decreasing")

	statement = paste0("(Y", "[", X, "=1] < ", Y, "[", X, "=0])")

	class(statement) <- "statement"

	statement
}


#' Make statement for any interaction
#'
#' Generate a statement for X1, X1 interact in the production of Y
#'
#' @param X1 input variable 1
#' @param X2 input variable 2
#' @param Y outcome variable
#' @family statements
#' @export
#' @return A character statement of class statement
#' @examples
#' interacts("A", "B", "Y")
#' get_types(model = make_model("X-> Y <- W"),
#'          query = interacts("X", "W", "Y"))
#'

interacts <- function(X1, X2, Y){

	if(!is.character(X1) | !is.character(X2) |!is.character(Y)) stop("Provide node names as strings in function: interacts")

	statement = paste0("((", Y, "[", X1, " =1, ", X2, " = 1]) - (", Y, "[",X1, " = 0, ", X2, " = 1])) != ",
										 "((", Y, "[", X1, " =1, ", X2, " = 0]) - (", Y, "[",X1, " = 0, ", X2, " = 0]))" )

	class(statement) <- "statement"

	statement
}



#' Make statement for complements
#'
#' Generate a statement for X1, X1 complement each other in the production of Y
#'
#' @param X1 input variable 1
#' @param X2 input variable 2
#' @param Y outcome variable
#' @family statements
#' @export
#' @return A character statement of class statement
#' @examples
#'complements("A", "B", "Y")
#'
complements <- function(X1, X2, Y){

	if(!is.character(X1) | !is.character(X2) |!is.character(Y)) stop("Provide node names as strings in function: complements")

	statement = paste0("((", Y, "[", X1, " =1, ", X2, " = 1]) - (", Y, "[",X1, " = 0, ", X2, " = 1])) > ",
										 "((", Y, "[", X1, " =1, ", X2, " = 0]) - (", Y, "[",X1, " = 0, ", X2, " = 0]))" )

	class(statement) <- "statement"

	statement
}



#' Make statement for substitutes
#'
#' Generate a statement for X1, X1 substitue for each other in the production of Y
#'
#' @param X1 input variable 1
#' @param X2 input variable 2
#' @param Y outcome variable
#' @export
#' @family statements
#' @return A character statement of class statement
#' @examples
#'
#'get_types(model = make_model("A -> B <- C"),
#'          query = substitutes("A", "C", "B"))
#'
#'query_model(model = make_model("A -> B <- C"),
#'          queries = substitutes("A", "C", "B"),
#'          using = "parameters")
#
substitutes <- function(X1, X2, Y){

	if(!is.character(X1) | !is.character(X2) |!is.character(Y)) stop("Provide node names as strings in function: substitutes")

	statement = paste0("((", Y, "[", X1, " = 1, ", X2, " = 1]) - (", Y, "[",X1, " = 0, ", X2, " = 1])) < ",
										 "((", Y, "[", X1, " = 1, ", X2, " = 0]) - (", Y, "[",X1, " = 0, ", X2, " = 0]))" )

	class(statement) <- "statement"

	statement
}

