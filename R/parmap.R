#' Make parmap: a matrix mapping from parameters to data types
#'
#' Generates a matrix with a row per parameter and a column per data type.
#'
#'@inheritParams CausalQueries_internal_inherit_params
#' @return A matrix
#' @export
#' @examples
#' make_parmap(model = make_model('X->Y'))
#' make_parmap(model = make_model('X->Y; X<->Y'))
#' make_parmap(model = make_model('X->Y; X<->Y')) %>% attr("map")
#' make_parmap(model = make_model('X -> M -> Y; X <-> Y'))
#' make_parmap(model = make_model('X -> M -> Y; M <-> Y'))
#' model <- make_model('X -> M -> Y; M <-> Y; X <-> M')
#' make_parmap(model)
#' make_parmap(model) %>% attr("map")
#' # Any ways (without paths splits)
#' make_parmap(model) %*% (make_parmap(model) %>% attr("map"))
#'
#' \dontrun{
#' # X1 and X2 are confounded and jointly determine Y1, Y2.
#' # For instance for models in which X and Y take on four values rather than 2.
#' model <- make_model("Y2 <- X1 -> Y1; Y2 <- X2 ->Y1; X1 <-> X2; Y1 <-> Y2")
#' parmap <- make_parmap(model)
#' parmap |> dim()
#'
#' CausalQueries:::prep_stan_data(
#'   model,
#'   CausalQueries:::minimal_event_data(model))$n_params
#' }

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
    return(type_matrix)
  }

  # If confounding, parmap has to split according to
  # distinct conditioning paths
  data_names <- colnames(A)[A %*% (1:ncol(A))]

  type <- apply(P, 2, function(j) {
    paste(model$parameters_df$given[j == 1], collapse = " ")
  })

  .type_matrix <- t(P) %>%
    data.frame() %>%
    mutate(type = type, d = data_names) %>%
    group_by(d, type) %>% summarize_all(max) %>%
    ungroup

  # reorder -- so that order is consistent with
  .type_matrix <- .type_matrix[order(match(.type_matrix$d, colnames(A))), ]


  type_matrix <- .type_matrix %>%
    select(-d, -type) %>%
    t()

  colnames(type_matrix) <- .type_matrix$d

  # type_matrix <- type_matrix[,match(colnames(type_matrix), colnames(A))]
  attr(type_matrix, "map") <- data_to_data(type_matrix, A)
  type_matrix
}

#' helper to generate a matrix mapping from names of M to names of A
#' @param M a matrix
#' @param A a matrix
#' @return a matrix
#' @keywords internal

data_to_data <- function(M, A){
    dnames <- colnames(A)
    out <- sapply(colnames(M), function(j) dnames %in% j )*1
    rownames(out) <- dnames
    out %>% t
}



#' Get parmap: a matrix mapping from parameters to data types
#'
#' Gets parmap from a model, or generates if not available.
#'
#'@inheritParams CausalQueries_internal_inherit_params
#' @return A matrix
#' @export
#' @examples
#' get_parmap(model = make_model('X->Y'))
#'
get_parmap <- function(model, A = NULL, P = NULL){
    if(!is.null(model$parmap)) return(model$parmap)
    make_parmap(model, A, P)
}


#' Set parmap: a matrix mapping from parameters to data types
#'
#' Generates and adds parmap to a model
#'
#'@inheritParams CausalQueries_internal_inherit_params
#' @return A matrix
#' @export
#' @examples
#' set_parmap(model = make_model('X->Y'))
#'
set_parmap <- function(model) {
    model$parmap <- make_parmap(model)
    model
}
