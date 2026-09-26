#' Nodal-type → child lookup (M1)
#'
#' Same encoding as \code{realise_outcomes_c} / \code{nodal_type_parent_index}.
#' Reuses \code{nodal_type_parent_index}; does not copy the bit formula.
#'
#' @keywords internal
#' @noRd
NULL

#' Child value from nodal type string and parent 0/1 values.
#' @keywords internal
#' @noRd
child_value_from_nodal_type <- function(nodal_type, parent_values) {
  nodal_type <- as.character(nodal_type)[[1]]
  if (!length(parent_values)) {
    return(as.integer(nodal_type))
  }
  pos <- nodal_type_parent_index(parent_values) + 1L
  if (nchar(nodal_type) < pos) {
    stop("Nodal type shorter than parent index.", call. = FALSE)
  }
  as.integer(substr(nodal_type, pos, pos))
}

#' Realise one world from a named list/row of nodal types and constant dos.
#' @keywords internal
#' @noRd
realise_world_from_types <- function(model, type_row, dos = NULL) {
  parents <- get_parents(model)
  values <- setNames(rep(NA_integer_, length(model$nodes)), model$nodes)
  dos <- if (is.null(dos)) list() else dos
  for (v in model$nodes) {
    if (v %in% names(dos)) {
      values[[v]] <- as.integer(dos[[v]])
      next
    }
    tau <- as.character(type_row[[v]])
    pa <- parents[[v]]
    if (!length(pa)) {
      values[[v]] <- as.integer(tau)
    } else {
      pv <- vapply(pa, function(p) as.integer(values[[p]]), integer(1))
      values[[v]] <- child_value_from_nodal_type(tau, pv)
    }
  }
  values
}
