
#' Lookup nodal types according to a query
#'
#' @inheritParams gbiqq_internal_inherit_params
#' @export
#' @importFrom stringr str_split str_detect
#' @importFrom dplyr select
#' @return A list containing the types and the evaluated expression. `manipulated_outcomes` are the nodes on the left of a [] expression
#' @examples
#' model <- make_model('X->Y')
#'
#' lookup_nodal_type(model, '(Y[X=0] > Y[X=1])')
#' lookup_nodal_type(model, '(Y[X=0] >= Y[X=1])')
#'
#' model <- make_model('X -> M -> Y; X->Y')
#' query <- '(Y[X=0] > Y[X=1])'
#' x <- lookup_nodal_type(model, query)
#'
#' query <- '(Y[X=0, M = .] > Y[X=1, M = 0])'
#' x <- lookup_nodal_type(model, query)
#'
#' query <- '(Y[] == 1)'
#' x <- lookup_nodal_type(model, query)
#' x <- lookup_nodal_type(model, query, join_by = '&')
#'
#' \dontrun{
#' query <- '(X == 1)'
#' x <- lookup_nodal_type(model, query)
#' }
#'
#' query <- '(M[X=1] == M[X=0])'
#' x <- lookup_nodal_type(model, query)
#'
#' # Complements
#' model <- make_model('M->Y; X->Y')
#' query <- complements('X', 'M', 'Y')
#' lookup_nodal_type(model, query)

lookup_nodal_type <-  function(model, query, join_by = "|") {

    # Get outcome var (preceding square brackets)
    node <- unique(st_within(query))
    if(length(node)>1) stop(paste0("Can't lookup types for nodes ", paste0(node, collapse = ", "),
                                   " simultaneously. Please write expression as separate queries"))
    # Expand causal expression
    expanded_query <- expand_nodal_expression(model, query, node, join_by = join_by)
    Q <- query_to_expression(expanded_query)

    # Xs: possible values of parents (no restrictions)
    pa <- get_parents(model)[node][[1]]
    Xs <- perm(rep(1,length(pa)))
    names(Xs) <- pa

    # Y: potential outcomes: possible nodal types (restrictions respected)
    assign(node, t(data.frame(get_nodal_types(model, collapse = FALSE)[node])))

    # Magic: evaluate the query expression on potential outcomes
    types <- with(Xs, eval(parse(text = Q)))

    # Output
    return_list <- list(types = types,
                        query = query,
                        expanded_query = expanded_query,
                        evaluated_nodes = t(eval(parse(text = node))),
                        node = node)

    class(return_list) <- "nodal_types"
    return(return_list)

}



#' @export
print.nodal_types <- function(x, ...) {
    print(summary(x))
    invisible(x)
}


#' @export
summary.nodal_types <- function(object, ...) {
    structure(object, class = c("summary.nodal_types", "data.frame"))

}

#' @export
print.summary.nodal_types <- function(x, ...) {
    output_type <- class(x$types)
    types_labels <- names(x$types)[x$types]
    nt <- length(types_labels)
    cat(paste("\nNodal types satisfying query's condition(s)"))

    if (x$query != x$expanded_query)
        cat(paste("\n\n query : ", x$expanded_query, "\n\n")) else cat(paste("\n\n query : ", x$query, "\n\n"))

    if (length(types_labels)%%2 != 0) {
        types_labels[length(types_labels) + 1] <- ""
    }
    counter <- 2
    while (counter <= length(types_labels)) {
        cat(paste0(" ", types_labels[(counter - 1):counter], collapse = "  "))
        cat("\n")
        counter <- counter + 2
    }

    cat(paste("\n\n Number of nodal types that meet query = ", nt))
    cat(paste("\n Total number of nodal types related to", x$node, "= ", length(x$types)))

}


#' Helper to fill in missing do operators in causal expression
#'
#' @param q a causal query
#' @param model a model
#' @examples
#' model <- make_model('X -> Y <- M')
#' gbiqq:::add_dots('Y[X=1]', model)
#' gbiqq:::add_dots('Y[]', model)
#'
add_dots <- function(q, model) {

    # Skip numeric strings------------------------------- 'query = `Y[] == 1` would be splitted as
    # c(`Y[]`, 1) and 1 doesn't need to be processed '
    if (!grepl("\\D", q))
        return(q)

    var <- st_within(q)
    if (!all(var %in% model$nodes))
        stop(paste0("Outcome node "), var, " not in model")

    # Only allow specification of var's parents

    # Identify parents not specified in query and paste them as 'parent = .'
    v_parents <- get_parents(model)[[var]]
    parents_in_q <- gbiqq:::nodes_in_statement(v_parents, q)
    not_parents <- gbiqq:::list_non_parents(model, var)
    not_parents_q <- gbiqq:::nodes_in_statement(not_parents, q)
    missing_parents <- v_parents[!v_parents %in% parents_in_q]

    if (length(not_parents_q) > 0) {
        conjugation <- ifelse(length(not_parents_q) > 1, "are not parents of", "is not a parent of")
        subjects <- paste0(not_parents_q, collapse = ", ")
        stop(paste(subjects, conjugation, var))
    }

    node <- var

    # Add wildcard if needed
    if (length(v_parents) != length(parents_in_q))
        q <- gbiqq:::add_wildcard(node, statement = q, parents = v_parents, missing_parents)

    q

}


#' Helper to expand nodal expression

expand_nodal_expression <- function(model, query, node, join_by = "|")	{

    operators <- "\\==|\\+|\\-|>=|<=|>|<|!=|\\&|\\|"

    # Add dots to query parts
    w_query <- gsub("\\(|\\)", "", query) %>%
        stringr::str_split(operators) %>%
        unlist %>%
        sapply(function(x) trimws(x)) %>%
        sapply(gbiqq:::add_dots, model = model)
    w_query <- gsub(" ", "", w_query)

    # Rejoin to complete query
    for (i in 1:length(w_query)) {
        string_i <- gsub(paste0("\\[|\\]|", node), "", names(w_query)[[i]])
        string_i <- paste0(node, "\\[", string_i, "\\]")
        string_i <- gsub(" ", "", string_i)
        query <- gsub(string_i, w_query[i], query)
    }

    # Expand
    gbiqq:::expand_wildcard(query, join_by = join_by, verbose = FALSE)
}

#' Helper to turn query into a data expression
query_to_expression <- function(query, node){

    query <- gsub("=","==", query)
    query <- gsub("===","==", query)
    query <- gsub("===","==", query) # errors if only provided once
    query <- gsub(">==",">=", query)
    query <- gsub("<==","<=", query)
    query <- gsub("!==","!=", query)
    query <- gsub("~==","~=", query)
    query <- gsub(","," & ", query)
    query <- gsub("\\]", ", \\]", query)
    query
}

