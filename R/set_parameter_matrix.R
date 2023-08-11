#' Make parameter matrix
#'
#' Calculate parameter matrix assuming no confounding. The parameter matrix maps from parameters into causal types. In models without confounding parameters correspond to nodal types.
#'
#' @inheritParams CausalQueries_internal_inherit_params
#' @return A \code{data.frame}, the parameter matrix, mapping from parameters to causal types
#' @export
#' @examples
#' model <- make_model('X -> Y')
#' make_parameter_matrix(model)

make_parameter_matrix <- function(model) {

    types <- causal_type_names(get_causal_types(model))
    pars <- paste0(model$parameters_df$node, model$parameters_df$nodal_type)

    # Which nodal_types correspond to a type
     P <- 1* apply(types, 1, function(j) pars %in% j) |>
       data.frame()

    # Tidy up
    colnames(P) <- rownames(types)
    rownames(P) <- model$parameters_df$param_names
    P <- as.data.frame(P)
    class(P) <- c("parameter_matrix", "data.frame")
    P
}

#' Get parameter matrix
#'
#' Return parameter matrix if it exists; otherwise calculate it assuming no confounding. The parameter matrix  maps from parameters into causal types. In models without confounding parameters correspond to nodal types.
#'
#' @param model A model created by \code{make_model()}
#' @return A \code{data.frame}, the parameter matrix, mapping from parameters to causal types
#' @export
#' @examples
#' model <- make_model('X -> Y')
#' get_parameter_matrix(model)
get_parameter_matrix <- function(model) {

    if (!is.null(model$P))
        return(model$P)
    return(make_parameter_matrix(model))
}


#' Set parameter matrix
#'
#' Add a parameter matrix to a model
#'
#' @inheritParams CausalQueries_internal_inherit_params
#' @return An object of class \code{causal_model}. It essentially returns a list containing the elements comprising
#' a model (e.g. 'statement', 'nodal_types' and 'DAG') with the parameter matrix attached to it.
#' @export
#'
#' @examples
#' model <- make_model('X -> Y')
#' P <- diag(8)
#' colnames(P) <- rownames(model$causal_types)
#' model <- set_parameter_matrix(model, P = P)
set_parameter_matrix <- function(model, P = NULL) {

  if(is.null(P)) {
    P <- make_parameter_matrix(model)
  }
  model$P <- P

  return(model)
}


#' @export
print.parameter_matrix <- function(x, ...) {
    print(summary(x))
    invisible(x)
}


#' @export
summary.parameter_matrix <- function(object, ...) {
    structure(object, class = c("summary.parameter_matrix", "data.frame"))

}

#' @export
print.summary.parameter_matrix <- function(x, ...) {
    cat(paste0("\nRows are parameters, grouped in parameter sets"))
    cat(paste0("\n\nColumns are causal types"))
    cat(paste0("\n\nCell entries indicate whether a parameter probability is used\nin the calculation of causal type probability\n\n"))

    param_set <- attr(x, "param_set")
    class(x) <- "data.frame"
    print(x)
    cat("\n \n param_set  (P)\n ")
    cat(paste0(param_set, collapse = "  "))
}



#' Names for causal types
#' @param causal_types A \code{data.frame} whose rows containing the 0-1 digits that conform the causal types.
#' @keywords internal
#' @return A \code{data.frame} whose rows contain the character values that conform each causal type in a model.
#' @examples
#' \donttest{
#' model <- make_model('X -> Y')
#' possible_types <- get_nodal_types(model)
#' df <- data.frame(expand.grid(possible_types, stringsAsFactors = FALSE))
#' CausalQueries:::causal_type_names(df)
#' }

causal_type_names <- function(causal_types) {
    for (j in (1:ncol(causal_types))) causal_types[, j] <- paste0(names(causal_types)[j], causal_types[,
        j])
    data.frame(causal_types, stringsAsFactors = FALSE)
}

