#' Model-level derived-object cache
#'
#' \code{model$.cache} is an environment attached at \code{make_model} so
#' in-place writes persist for the caller. Mutators must call
#' \code{clear_model_cache()}. Used for \code{realise_outcomes} memoization
#' and other derived objects.
#'
#' @keywords internal
#' @noRd
NULL

#' Attach an empty cache environment on a model (idempotent).
#' Attached in \code{make_model} so later in-place writes into the
#' environment persist for the caller without returning the model.
#' @keywords internal
#' @noRd
ensure_model_cache <- function(model) {
  if (is.null(model$.cache) || !is.environment(model$.cache)) {
    model$.cache <- new.env(parent = emptyenv())
  }
  model
}

#' Drop all derived cache entries (call from every mutator).
#' @keywords internal
#' @noRd
clear_model_cache <- function(model) {
  if (!is.null(model$.cache) && is.environment(model$.cache)) {
    rm(list = ls(envir = model$.cache, all.names = TRUE), envir = model$.cache)
  }
  model
}

#' Fingerprint of the causal-type schedule used by \code{realise_outcomes}.
#' Factorized query attaches a relevant subset on a shallow model copy; the
#' key must distinguish that from the full schedule or cache hits poison
#' estimands (\code{W[, g]} length mismatch).
#' @keywords internal
#' @noRd
causal_types_cache_fp <- function(model) {
  ct <- model$causal_types
  if (is.null(ct)) {
    return("auto")
  }
  rn <- rownames(ct)
  if (is.null(rn)) {
    return(paste0("n", nrow(ct)))
  }
  # Compact: size + endpoints (rownames uniquely identify the grid)
  paste0("n", nrow(ct), ":", rn[[1L]], ":", rn[[length(rn)]])
}

#' Stable key for a \code{dos} list (scalars or per-type vectors).
#' @keywords internal
#' @noRd
dos_cache_key <- function(dos = NULL,
                          node = NULL,
                          add_rownames = TRUE,
                          types_fp = "auto") {
  if (is.null(dos) || !length(dos)) {
    dpart <- "."
  } else {
    nm <- sort(names(dos))
    dpart <- paste(vapply(nm, function(n) {
      v <- dos[[n]]
      paste0(n, "=", paste(as.character(v), collapse = ","))
    }, character(1)), collapse = "|")
  }
  paste(dpart, if (is.null(node)) "." else node, as.integer(add_rownames),
        types_fp, sep = "/")
}
