#' Make parmap: a matrix mapping from parameters to data types
#'
#' Generates a matrix with a row per parameter and a column per data type.
#'
#'@inheritParams CausalQueries_internal_inherit_params
#' @return A matrix

make_parmap <- function(model, A = NULL, P = NULL) {
  d <- NULL

  if (is.null(A)) {
    A <- get_ambiguities_matrix(model)
  }

  if (is.null(P)) {
    P <- get_parameter_matrix(model)
  }

  # if no confounding, parmap is just P*A
  if (!grepl("<->", model$statement)) {
    type_matrix <- 1 * ((as.matrix(P) %*% as.matrix(A)) > 0)
    map <- diag(ncol(type_matrix))
    rownames(map) <- colnames(map) <- colnames(A)
    attr(type_matrix, "map") <- map
    class(type_matrix) <- class(type_matrix)
    return(type_matrix)
  }

  # If confounding, parmap has to split according to
  # distinct conditioning paths
  data_names <- colnames(A)[A %*% (seq_len(ncol(A)))]

  type <- apply(P, 2, function(j) {
    paste(model$parameters_df$given[j == 1], collapse = " ")
  })

  .type_matrix <- t(P) |>
    data.frame() |>
    mutate(type = type, d = data_names) |>
    group_by(d, type) |> summarize_all(max) |>
    ungroup()

  # reorder -- so that order is consistent with
  .type_matrix <- .type_matrix[order(match(.type_matrix$d, colnames(A))), ]


  type_matrix <- .type_matrix |>
    select(-d, -type) |>
    t()

  colnames(type_matrix) <- .type_matrix$d

  attr(type_matrix, "map") <- data_to_data(type_matrix, A)
  class(type_matrix) <- class(type_matrix)
  type_matrix
}

#' helper to generate a matrix mapping from names of M to names of A
#' @param M a matrix
#' @param A a matrix
#' @return a matrix
#' @keywords internal

data_to_data <- function(M, A){
  dnames <- colnames(A)
  out <- vapply(colnames(M), function(j) {
    as.integer(dnames %in% j)
  }, numeric(length(dnames)))
  rownames(out) <- dnames
  t(out)
}


#' Get parmap: a matrix mapping from parameters to data types
#'
#' Gets parmap from a model, or generates if not available.
#'
#'@inheritParams CausalQueries_internal_inherit_params
#' @return A matrix

get_parmap <- function(model, A = NULL, P = NULL){
    if(!is.null(model$parmap)) {
      return(model$parmap)
    }
    make_parmap(model, A, P)
}


#' Set parmap: a matrix mapping from parameters to data types
#'
#' Generates and adds parmap to a model
#'
#'@inheritParams CausalQueries_internal_inherit_params
#' @return A matrix

set_parmap <- function(model) {
    model$parmap <- make_parmap(model)
    model
}
