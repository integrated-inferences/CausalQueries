
#' Find which nodal types satisfy a query
#'
#' @inheritParams CausalQueries_internal_inherit_params
#' @noRd
#' @keywords internal
#' @importFrom stringr str_split str_detect
#' @importFrom dplyr select
#' @return A \code{list} containing the types and the evaluated expression.
#'   `manipulated_outcomes` are the nodes on the left of a [] expression.
#'
#' @examples
#' \donttest{
#' model <- make_model('X->Y')
#'
#' CausalQueries:::map_query_to_nodal_type(model, '(Y[X=0] > Y[X=1])')
#' CausalQueries:::map_query_to_nodal_type(model, '(Y[X=0] >= Y[X=1])')
#'
#' model <- make_model('X -> M -> Y; X->Y')
#' query <- '(Y[X=0] > Y[X=1])'
#' x <- CausalQueries:::map_query_to_nodal_type(model, query)
#'
#' query <- '(Y[X=0, M = .] > Y[X=1, M = 0])'
#' x <- CausalQueries:::map_query_to_nodal_type(model, query)
#'
#' query <- '(Y[] == 1)'
#' x <- CausalQueries:::map_query_to_nodal_type(model, query)
#' x <- CausalQueries:::map_query_to_nodal_type(model, query, join_by = '&')
#'
#' # Root nodes specified with []
#' CausalQueries:::map_query_to_nodal_type(model, '(X[] == 1)')
#' }
#' \dontrun{
#' CausalQueries:::map_query_to_nodal_type(model, 'X == 1')
#' }
#'
#' query <- '(M[X=1] == M[X=0])'
#' x <- CausalQueries:::map_query_to_nodal_type(model, query)
#'
#' # Complements
#' model <- make_model('M->Y; X->Y')
#' query <- complements('X', 'M', 'Y')
#' CausalQueries:::map_query_to_nodal_type(model, query)
#'
#' # Complex queries
#' model <- make_model('X->Y')
#' CausalQueries:::map_query_to_nodal_type(model, te("X", "Y"))

map_query_to_nodal_type <-  function(model, query, join_by = "|") {

    # check if query has been properly specified
    query <- check_query(query)
    # Get outcome var (preceding square brackets)
    node <- unique(st_within(query))

    if (is.na(node[1])) {
      stop(
        paste(
          "No outcome variable specified.",
          "If required, specify roots as X[]==1 not X==1."
        )
      )
    }

    if (length(node) > 1) {
      stop(
        paste0(
          "Can't lookup types for nodes ",
          paste0(node, collapse = ", "),
          " simultaneously. Please write expression as separate queries"
        )
      )
    }

    # Xs: possible values of parents (no restrictions)
    pa <- get_parents(model)[node][[1]]
    Xs <- perm(rep(1,length(pa)))
    names(Xs) <- pa

    expanded_query <- expand_nodal_expression(model,
                                              query,
                                              node,
                                              join_by = join_by)
    Q <- query_to_expression(expanded_query)


    # Y: potential outcomes: possible nodal types (restrictions respected)
    # dataset assigned to "node"
    assign(node, get_nodal_types(model, collapse = FALSE)[[node]] |> t())

    # Magic: evaluate the query expression on potential outcomes
    # This is a hard line; we use different values of Xs to pick out
    # columns of Y potential outcomes (node)
    types <- with(Xs, eval(parse(text = Q)))

    # Add name for singletons
    if(length(types) == 1 && is.null(names(types))) {
      names(types) <- model$nodal_types[node]
    }

    # Output
    return_list <- list(types = types,
                        query = query,
                        expanded_query = expanded_query,
                        evaluated_nodes = t(eval(parse(text = node))),
                        node = node)

    class(return_list) <- "nodal_types_query"
    return(return_list)

}


#' @export
print.nodal_types_query <- function(x, ...) {
  if (length(unique(x$types)) > 2) {
    cat(
      paste(
        "\n\n Caution: This appears to be a complex query reporting",
        "coefficients on types and not simply identifying types\n\n"
      )
    )
  }

  output_type <- class(x$types)

  types_labels <- names(x$types)[x$types != 0]
  nt <- length(types_labels)
  cat(paste("\nNodal types adding weight to query"))

  if (x$query != x$expanded_query) {
    cat(paste("\n\n query : ", x$expanded_query, "\n\n"))
  } else {
    cat(paste("\n\n query : ", x$query, "\n\n"))
  }

  if (length(types_labels) %% 2 != 0) {
    types_labels[length(types_labels) + 1] <- ""
  }
  counter <- 2
  while (counter <= length(types_labels)) {
    cat(paste0(" ", types_labels[(counter - 1):counter], collapse = "  "))
    cat("\n")
    counter <- counter + 2
  }

  cat(paste(
    "\n\n Number of nodal types that add weight to query =",
    nt))

  cat(paste(
    "\n Total number of nodal types related to",
    x$node,
    "=",
    length(x$types)
  ))
  invisible(x)
}



#' Helper to fill in missing do operators in causal expression
#' @inheritParams CausalQueries_internal_inherit_params
#' @param q A character string. Causal query with at least one parent node
#'   missing their do operator.
#' @keywords internal
#' @return A causal query expression with all parents nodes set to
#'   either 0, 1 or wildcard '.'.
#' @examples
#' \donttest{
#' model <- make_model('X -> Y <- M')
#' CausalQueries:::add_dots('Y[X=1]', model)
#' CausalQueries:::add_dots('Y[]', model)
#'}

add_dots <- function(q, model) {
  # Skip numeric strings-------------------------------
  # 'query = `Y[] == 1` would be split as
  # c(`Y[]`, 1) and 1 doesn't need to be processed '
  if (!grepl("\\D", q)) {
    return(q)
  }

  var <- st_within(q)
  if (!all(var %in% model$nodes)) {
    stop(paste0("Outcome node "), var, " not in model")
  }

  # Only allow specification of var's parents
  # Identify parents not specified in query and paste them as 'parent = .'
  v_parents <- get_parents(model)[[var]]
  parents_in_q <- nodes_in_statement(v_parents, q)
  not_parents <- list_non_parents(model, var)
  not_parents_q <- nodes_in_statement(not_parents, q)
  missing_parents <- v_parents[!v_parents %in% parents_in_q]

  if (length(not_parents_q) > 0) {
    conjugation <- ifelse(length(not_parents_q) > 1,
                          "are not parents of",
                          "is not a parent of")
    subjects <- paste0(not_parents_q, collapse = ", ")
    stop(paste(subjects, conjugation, var))
  }

  node <- var

  # Add wildcard if needed
  if (length(v_parents) != length(parents_in_q)) {
    q <- add_wildcard(node,
                      statement = q,
                      parents = v_parents,
                      missing_parents)
  }

  return(q)
}


#' Helper to expand nodal expression
#' @keywords internal
#' @return A nodal expression with no missing parents
#' @inheritParams CausalQueries_internal_inherit_params

expand_nodal_expression <- function(model,
                                    query,
                                    node,
                                    join_by = "|")	{
  operators <- "\\==|\\+|\\-|>=|<=|>|<|!=|\\&|\\|"
  query <- gsub(" ", "", query)

  # Add dots to query parts
  w_query <- gsub("\\(|\\)", "", query) |>
    stringr::str_split(operators) |>
    unlist() |>
    vapply(function(x) {
      trimws(x)
    }, character(1)) |>
    vapply(add_dots, model = model, character(1))
  w_query <- gsub(" ", "", w_query)

  # Rejoin to complete query
  for (i in seq_along(w_query)) {
    string_i <- gsub(paste0("\\[|\\]|", node), "", names(w_query)[[i]])
    string_i <- paste0(node, "\\[", string_i, "\\]")
    string_i <- gsub(" ", "", string_i)
    query <- gsub(string_i, w_query[i], query)
  }

  # Expand
  if (grepl("\\.", query)) {
    query <- expand_wildcard(query, join_by = join_by, verbose = FALSE)
  }

  return(query)
}

#' Helper to turn query into a data expression
#' @keywords internal
#' @return A cleaned query expression
#' @inheritParams CausalQueries_internal_inherit_params
query_to_expression <- function(query, node){
    query <- gsub("=","==", query)
    query <- gsub("====","==", query)
    query <- gsub(">==",">=", query)
    query <- gsub("<==","<=", query)
    query <- gsub("!==","!=", query)
    query <- gsub("~==","~=", query)
    query <- gsub(","," & ", query)
    query <- gsub("\\]", ", \\]", query)
    return(query)
}

