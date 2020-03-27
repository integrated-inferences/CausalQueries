
#' Get values of types according to a query
#'
#'@inheritParams CausalQueries_internal_inherit_params
#'
#' @export
#' @return A \code{list} containing the types and the evaluated expression.
#' @examples
#' model <- make_model('X -> M -> Y; X->Y')
#' query <- '(Y[X=1] > Y[X=0]) & (M[X=0]==1)'
#' x <- map_query_to_causal_type(model, query)
#' summary(x)
#'
#' query <- 'Y[M=M[X=0], X=1]==1'
#' x <- map_query_to_causal_type(model, query)
#' map_query_to_causal_type(model, query)
#'
#' query <- '(Y[X=1, M = 1] >  Y[X=0, M = 1]) & (Y[X=1, M = 0] >  Y[X=0, M = 0])'
#' map_query_to_causal_type(model, query)
#'
#' query <- 'Y[X=1] == Y[X=0]'
#' map_query_to_causal_type(model, query)
#'
#' query <- '(X == 1) & (M==1) & (Y ==1) & (Y[X=0] ==1)'
#' x <- map_query_to_causal_type(model, query)
#'
#' query <- '(Y[X = .]==1)'
#' map_query_to_causal_type(model, query)
#'
#' model <- make_model('X -> Y')

map_query_to_causal_type <- function(model, query, join_by = "|") {

    if (length(query) > 1L)
        stop("Please specify a query of length 1L.")

    if (grepl(".", query, fixed = TRUE))
        query <- expand_wildcard(query, join_by = join_by)

    # Global Variables
    i <- 0
    list_names <- ""
    continue <- TRUE

    # strip whitespaces split query into single characters locate opening brackets and reverse oder
    w_query <- gsub(" ", "", query)
    w_query <- unlist(strsplit(query, ""))
    bracket_starts <- rev(grep("\\[", w_query))
    bracket_ends <- rev(grep("\\]", w_query))

    if (length(bracket_starts) != length(bracket_ends)) {
        stop("Either '[' or ']' missing.")
    }
    if (length(bracket_starts) == 0) {
        continue = FALSE
    }

    eval_var <- reveal_outcomes(model)

    list_names <- colnames(eval_var)
    k <- ncol(eval_var) + 1

    while (continue) {
        i <- i + 1

        # start at the latest found '[' find the closest subsequent ']' remove brackets and extract
        # expression
        .query <- w_query[(bracket_starts[i]):length(w_query)]
        .bracket_ends <- grep("\\]", .query)[1]
        .query <- .query[1:.bracket_ends]
        brackets <- grepl("\\[|\\]", .query)
        .query <- .query[!brackets]

        # Split expression by ',' 'x = 1, m = 0' into 'x = 1' 'm=0'
        .query <- paste0(.query, collapse = "")
        .query <- unlist(strsplit(.query, ","))
        dos <- list()

        # Walks through splitted expressions (i.e dos) and evaulates each expression when possible
        if(length(.query) == 0) {
            stop("\nquery does not return any causal types.\nNote that expressions of the form `Y[]==1` are not allowed for mapping queries to causal types.\nSpecify queries as (e.g.) `Y==1` or `Y[X=0] == 1` instead.")
        }
        for (j in 1:length(.query)) {
            do <- unlist(strsplit(.query[j], ""))
            stop <- gregexpr("=", .query[j], perl = TRUE)[[1]][1] - 1
            var_name <- paste0(do[1:stop], collapse = "")
            var_name <- gsub(" ", "", var_name)
            value <- c(eval(parse(text = paste0(do, collapse = "")), envir = eval_var))
            vars <- model$nodes
            if (!var_name %in% vars)
                stop(paste("Variable", var_name, "is not part of the model."))
            dos[[j]] <- value
            names(dos)[[j]] <- var_name
        }

        b <- 1:bracket_starts[i]
        var <- paste0(w_query[b], collapse = "")
        var <- st_within(var)
        var <- var[length(var)]

        # Save result from last iteration and remove corresponding expression w_query
        var_length <- nchar(var)
        data <- reveal_outcomes(model, dos)
        eval_var[, k] <- as.numeric(data[, var])


        .bracket_ends <- bracket_starts[i] + .bracket_ends - 1
        s <- seq(bracket_starts[i] - var_length, .bracket_ends)
        list_names[k] <- paste0(w_query[s], collapse = "")
        names(list_names)[k] <- names(eval_var)[k] <- w_query[s[1]] <- paste0("var", k)
        w_query[s[2:length(s)]] <- ""
        k <- k + 1

        # Stop loop there are no [] left
        if (!any(grep("\\[|\\]", w_query))) {
            continue <- FALSE
        }
    }  # end of [] application

    # Format the query as statement
    w_query <- {
        w_query <- paste0(w_query, collapse = "")
        w_query <- gsub(" ", "", w_query)
        paste0("q <- ", w_query)
    }

    # Magic
    types <- c(eval(parse(text = w_query), eval_var))

    # Clean up
    names(eval_var) <- list_names
    names(types) <- attr(eval_var, "type_names")

    if (is.logical(types))
        type_list <- names(types)[types]
    if (!is.logical(types))
        type_list <- NULL

    # Return
    return_list <- list(types = types, query = query, evaluated_nodes = eval_var, type_list = type_list)

    class(return_list) <- "causal_types"

    return_list

}

#' @export
print.causal_types <- function(x, ...) {
    print(summary(x))
    invisible(x)
}


#' @export
summary.causal_types <- function(object, ...) {
    structure(object, class = c("summary.causal_types", "data.frame"))

}

#' @export
print.summary.causal_types <- function(x, ...) {
    output_type <- class(x$types)
    if (output_type == "logical") {
        types1 <- x$types[x$types]
        cat(paste("\nCausal types satisfying query's condition(s)  \n\n query = ", x$query, "\n\n"))

        if (length(types1)%%2 != 0) {
            types1[length(types1) + 1] <- ""
        }
        counter <- 2
        while (counter <= length(types1)) {
            cat(paste0(names(types1[(counter - 1):counter]), collapse = "  "))
            cat("\n")
            counter <- counter + 2
        }

        cat(paste("\n\n Number of causal types that meet condition(s) = ", length(types1)))
        cat(paste("\n Total number of causal types in model = ", length(x$types)))
    } else if (output_type == "numeric") {
        print(x$types)
    } else {
        print(x)
    }

}


