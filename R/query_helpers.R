
#' Make monotonicity statement (positive)
#'
#' Generate a statement for Y monotonic (increasing) in X
#'
#' @param X A character. The quoted name of the input node
#' @param Y A character. The quoted name of the outcome node
#' @family statements
#' @export
#'
#' @return A character statement of class statement
#' @examples
#' increasing('A', 'B')
#'
increasing <- function(X, Y) {

    check_string_input(param_list = list(X, Y), call_name = deparse(sys.call()))

    statement = paste0("(", Y, "[", X, "=1] > ", Y, "[", X, "=0])")

    class(statement) <- "statement"

    statement
}


#' Make monotonicity statement (non negative)
#'
#' Generate a statement for Y weakly monotonic (increasing) in X
#'
#' @param X A character. The quoted name of the input node
#' @param Y A character. The quoted name of the outcome node
#' @family statements
#' @export
#'
#' @return A character statement of class statement
#' @examples
#' non_decreasing('A', 'B')
#'
non_decreasing <- function(X, Y) {

    check_string_input(param_list = list(X, Y), call_name = deparse(sys.call()))

    statement = paste0("(", Y, "[", X, "=1] >= ", Y, "[", X, "=0])")

    class(statement) <- "statement"

    statement
}


#' Make monotonicity statement (negative)
#'
#' Generate a statement for Y monotonic (decreasing) in X
#'
#' @param X A character. The quoted name of the input node
#' @param Y A character. The quoted name of the outcome node
#' @family statements
#' @export
#' @return A character statement of class statement
#' @examples
#' decreasing('A', 'B')
#'
decreasing <- function(X, Y) {

    check_string_input(param_list = list(X, Y), call_name = deparse(sys.call()))

    statement = paste0("(", Y, "[", X, "=1] < ", Y, "[", X, "=0])")

    class(statement) <- "statement"

    statement
}


#' Make monotonicity statement (non positive)
#'
#' Generate a statement for Y weakly monotonic (not increasing) in X
#'
#' @param X A character. The quoted name of the input node
#' @param Y A character. The quoted name of the outcome node
#' @family statements
#' @export
#' @return A character statement of class statement
#' @examples
#' non_increasing('A', 'B')
#'
non_increasing <- function(X, Y) {

    check_string_input(param_list = list(X, Y), call_name = deparse(sys.call()))

    statement = paste0("(", Y, "[", X, "=1] <= ", Y, "[", X, "=0])")

    class(statement) <- "statement"

    statement
}



#' Make statement for any interaction
#'
#' Generate a statement for X1, X1 interact in the production of Y
#'
#' @param X1 A character. The quoted name of the input node 1.
#' @param X2 A character. The quoted name of the input node 2.
#' @param Y A character. The quoted name of the outcome node.
#' @family statements
#' @export
#' @return A character statement of class statement
#' @examples
#' interacts('A', 'B', 'W')
#' map_query_to_causal_type(model = make_model('X-> Y <- W'),
#'          query = interacts('X', 'W', 'Y'))
#'

interacts <- function(X1, X2, Y) {

    check_string_input(param_list = list(X1, X2, Y), call_name = deparse(sys.call()))

    statement = paste0("((", Y, "[", X1, " =1, ", X2, " = 1]) - (", Y, "[", X1, " = 0, ", X2, " = 1])) != ",
        "((", Y, "[", X1, " =1, ", X2, " = 0]) - (", Y, "[", X1, " = 0, ", X2, " = 0]))")

    class(statement) <- "statement"

    statement
}



#' Make statement for complements
#'
#' Generate a statement for X1, X1 complement each other in the production of Y
#'
#' @param X1 A character. The quoted name of the input node 1.
#' @param X2 A character. The quoted name of the input node 2.
#' @param Y A character. The quoted name of the outcome node.
#' @family statements
#' @export
#' @return A character statement of class statement
#' @examples
#'complements('A', 'B', 'W')
#'
complements <- function(X1, X2, Y) {

    check_string_input(param_list = list(X1, X2, Y), call_name = deparse(sys.call()))

    statement = paste0("((", Y, "[", X1, " =1, ", X2, " = 1]) - (", Y, "[", X1, " = 0, ", X2, " = 1])) > ",
        "((", Y, "[", X1, " =1, ", X2, " = 0]) - (", Y, "[", X1, " = 0, ", X2, " = 0]))")

    class(statement) <- "statement"

    statement
}



#' Make statement for substitutes
#'
#' Generate a statement for X1, X1 substitue for each other in the production of Y
#'
#' @param X1 A character. The quoted name of the input node 1.
#' @param X2 A character. The quoted name of the input node 2.
#' @param Y A character. The quoted name of the outcome node.
#' @export
#' @family statements
#' @return A character statement of class statement
#' @examples
#'
#'map_query_to_causal_type(model = make_model('A -> B <- C'),
#'          query = substitutes('A', 'C', 'B'))
#'
#'query_model(model = make_model('A -> B <- C'),
#'          queries = substitutes('A', 'C', 'B'),
#'          using = 'parameters')
#
substitutes <- function(X1, X2, Y) {

    check_string_input(param_list = list(X1, X2, Y), call_name = deparse(sys.call()))

    statement = paste0("((", Y, "[", X1, " = 1, ", X2, " = 1]) - (", Y, "[", X1, " = 0, ", X2, " = 1])) < ",
        "((", Y, "[", X1, " = 1, ", X2, " = 0]) - (", Y, "[", X1, " = 0, ", X2, " = 0]))")

    class(statement) <- "statement"

    statement
}

#' Make treatment effect statement (positive)
#'
#' Generate a statement for (Y(1) - Y(0)). This statement when applied to a model returns an element in (1,0,-1) and not a set of cases. This is useful for some purposes such as querying a model, but not for uses that require a list of types, such as \code{set_restrictions}.
#'
#' @param X A character. The quoted name of the input node
#' @param Y A character. The quoted name of the outcome node
#' @family statements
#' @export
#'
#' @return A character statement of class statement
#' @examples
#' te('A', 'B')
#'
#' model <- make_model('X->Y') %>% set_restrictions(increasing('X', 'Y'))
#' query_model(model, list(ate = te('X', 'Y')),  using = 'parameters')
#'
#' # set_restrictions  breaks with te because it requires a listing
#' # of causal types, not numeric output.
#'\dontrun{
#' model <- make_model('X->Y') %>% set_restrictions(te('X', 'Y'))
#' }
#'
te <- function(X, Y) {
    check_string_input(param_list = list(X, Y), call_name = deparse(sys.call()))

    statement = paste0("(", Y, "[", X, "=1] - ", Y, "[", X, "=0])")

    class(statement) <- "statement"

    statement
}


#' Check string_input
#'
#' @param param_list List of parameters
#' @param call_name Name of the call.
#' @keywords internal
check_string_input <- function(param_list = list(), call_name = NULL) {
    for (i in 1:length(param_list)) if (!is.character(param_list[[i]]))
        stop(paste0("Provide node names as strings in function: ", call_name))
}




