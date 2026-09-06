#' Dual path: factorized (default) vs legacy causal types
#'
#' \describe{
#'   \item{\code{legacy = FALSE} (default)}{Parameters-only / factorized
#'     path: \code{make_model} does not attach a global causal-type table;
#'     update and query work from nodal parameter draws.}
#'   \item{\code{legacy = TRUE}}{Causal-type expansion, type-level maps,
#'     and Stan mixture likelihood.}
#' }
#'
#' **Inheritance.** The model carries the method used so far
#' (\code{model$legacy}; after updating, also
#' \code{model$stan_objects$legacy}). Later steps that depend on that object
#' (\code{update_model}, \code{query_*}, type-level draws) resolve
#' \code{legacy = NULL} from the model first, then
#' \code{options(CausalQueries.legacy)}. An explicit per-call
#' \code{legacy} that disagrees with the stamped model warns: do not mix
#' factorized and causal-type answers silently.
#'
#' **Draws.** Under both paths, prior / posterior draws of interest are
#' **parameters** (lambdas / simplexes). Expanding those draws into a full
#' causal-type weight matrix is a legacy convenience, not the factorized
#' default.
#'
#' @keywords internal
#' @noRd

default_legacy <- function() {
  opt <- getOption("CausalQueries.legacy", FALSE)
  normalize_legacy(opt)
}

normalize_legacy <- function(legacy) {
  if (is.null(legacy) || length(legacy) != 1 || is.na(legacy)) {
    stop("`legacy` must be a single logical TRUE or FALSE")
  }
  if (!is.logical(legacy)) {
    stop("`legacy` must be a single logical TRUE or FALSE")
  }
  legacy
}

#' Read stamped method from a model (or first model in a list).
#' @keywords internal
#' @noRd
get_model_legacy <- function(model = NULL) {
  if (is.null(model)) {
    return(NULL)
  }
  m0 <- if (is.list(model) && !is(model, "causal_model")) model[[1]] else model
  if (!is(m0, "causal_model")) {
    return(NULL)
  }
  # Prefer fit stamp when present (how the posterior was produced)
  if (!is.null(m0$stan_objects$legacy)) {
    return(normalize_legacy(m0$stan_objects$legacy))
  }
  if (!is.null(m0$legacy)) {
    return(normalize_legacy(m0$legacy))
  }
  NULL
}

#' Resolve legacy flag: explicit arg, else stamped model, else option / default.
#'
#' Warns when an explicit \code{legacy} disagrees with the model's stamp so
#' callers notice mixed-path use.
#'
#' @keywords internal
#' @noRd
resolve_legacy <- function(legacy = NULL, model = NULL) {
  stamped <- get_model_legacy(model)

  if (!is.null(legacy)) {
    out <- normalize_legacy(legacy)
    if (!is.null(stamped) && !identical(out, stamped)) {
      warning(
        "legacy = ", out,
        " overrides model stamp legacy = ", stamped, ". ",
        "Later steps usually should inherit the method used earlier ",
        "(leave legacy = NULL).",
        call. = FALSE
      )
    }
    return(out)
  }

  if (!is.null(stamped)) {
    return(stamped)
  }
  default_legacy()
}

#' Stamp the method used on this model (and on stan_objects after a fit).
#' @keywords internal
#' @noRd
stamp_legacy <- function(model, legacy) {
  legacy <- normalize_legacy(legacy)
  model$legacy <- legacy
  if (!is.null(model$stan_objects) || !is.null(model$posterior_distribution)) {
    if (is.null(model$stan_objects)) {
      model$stan_objects <- list()
    }
    model$stan_objects$legacy <- legacy
  }
  model
}

#' Clear error while a factorized feature is not yet supported.
#' Prefer domain-specific helpers (e.g. model_has_confound).
#' @keywords internal
#' @noRd
factorized_not_ready <- function(what = "update_model") {
  stop(
    "legacy = FALSE (factorized / parameters-only path) is not implemented yet ",
    "for ", what, ". ",
    "Pass legacy = TRUE to use the current causal-type method, or set ",
    "options(CausalQueries.legacy = TRUE).",
    call. = FALSE
  )
}

#' Message when type_posterior is requested but not stored.
#' @keywords internal
#' @noRd
type_posterior_unavailable_msg <- function(model) {
  stamped <- get_model_legacy(model)
  factorized <- !isTRUE(stamped)

  if (factorized) {
    paste0(
      "No type_posterior on this model. Factorized updates (legacy = FALSE) ",
      "keep parameter draws, not a full causal-type draw matrix.\n",
      "  - Parameter posterior: grab(model, \"posterior_distribution\")\n",
      "  - Posterior for a particular type or potential-outcome query: ",
      "query_model(model, query = \"...\", using = \"posteriors\")\n",
      "  - Type labels / parameter map (on demand): ",
      "grab(model, \"causal_types\"), grab(model, \"parameter_matrix\")\n",
      "  - Full draws-by-types matrix as in older releases: ",
      "update_model(..., legacy = TRUE, keep_type_distribution = TRUE)"
    )
  } else {
    paste0(
      "No type_posterior on this model. Re-run update_model() with ",
      "keep_type_distribution = TRUE."
    )
  }
}

# Back-compat aliases used briefly during the representation= rename
#' @keywords internal
#' @noRd
resolve_representation <- function(representation = NULL, model = NULL) {
  if (!is.null(representation)) {
    representation <- tolower(as.character(representation))
    if (representation %in% c("legacy", "factorized")) {
      return(if (identical(representation, "legacy")) "legacy" else "factorized")
    }
  }
  if (resolve_legacy(NULL, model)) "legacy" else "factorized"
}
