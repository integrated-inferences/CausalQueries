#' Simplify nodal types (drop interactions, impose monotonicity)
#'
#' Build or rebuild a model's nodal types without (necessarily) starting from a
#' saturated many-parent table and cutting with queries. Intended for large
#' parent sets: generate allowed schedules under interaction-order and
#' monotonicity rules, then attach them. When \code{drop_interactions} drops
#' order \eqn{\ge 2}, more than four parents are allowed because candidates are
#' built up (unary / kept blocks) rather than enumerated from the saturated set.
#'
#' \code{set_nodal_restrictions} is an alias of \code{simplify_model}.
#'
#' @param model A \code{causal_model}. For \code{make_model}, pass the same
#'   arguments there to apply at construction.
#' @param drop_interactions Drop interaction orders at least this high.
#'   \code{NULL} or \code{FALSE}: no interaction dropping.
#'   \code{TRUE} or \code{"all"}: drop order \eqn{\ge 2}.
#'   An integer \code{2}, \code{3}, or \code{4}: drop order \eqn{\ge} that value.
#'   A range such as \code{2:4} uses the minimum (drop \eqn{\ge 2}).
#' @param keep_interactions Exceptions: parent sets whose interactions are
#'   allowed even when their order would otherwise be dropped. A list of
#'   character vectors of parent names; optionally a named list by child
#'   (e.g. \code{list(Y = list(c("A", "B")))}).
#' @param monotone Monotonicity and related restrictions on parent effects.
#'   \code{NULL}: none. Prefer the **named list** form (no string parsing):
#'   \code{list(Y = c(A = "m", B = "+", C = "n"))}.
#'
#'   Codes for a given parent of a child (effect of raising that parent,
#'   holding other parents fixed):
#'   \describe{
#'     \item{\code{"+"}}{keep types that are weakly **increasing** in the parent
#'       (never a negative effect). Example two-parent strings kept:
#'       \code{"0011"} (Y equals X2), \code{"0001"} (AND); dropped:
#'       \code{"1100"} (Y equals not X2), \code{"1001"} (XNOR).}
#'     \item{\code{"-"}}{keep types that are weakly **decreasing** in the parent.
#'       Keeps \code{"1100"}; drops \code{"0011"} and \code{"1001"}.}
#'     \item{\code{"m"}}{keep types with **no qualitative interaction** in that
#'       parent: the effect never sign-changes across backgrounds (uniformly
#'       nondecreasing, uniformly nonincreasing, or flat). Keeps e.g.
#'       \code{"0011"} and \code{"1100"}; drops XOR/XNOR-style \code{"0110"} /
#'       \code{"1001"}. Weaker than \code{"+"} or \code{"-"} alone.}
#'     \item{\code{"n"}}{keep only types with a **qualitative interaction** in
#'       that parent (positive in some backgrounds and negative in others).
#'       Usual practice is \code{"m"} (exclude those types), not \code{"n"}.}
#'   }
#'
#'   Shorthands: \code{monotone = "+"}, \code{"-"}, \code{"m"}, or \code{"n"}
#'   applies that code to **every** parent of every endogenous node.
#'
#'   Compact strings such as \code{"A+Y"} or \code{"AmY"} are optional sugar
#'   (parent, code, child). You do **not** need them; use the list form if node
#'   names are unusual. Rare ambiguous strings (e.g. \code{"AmmY"} when both
#'   \code{Am -> Y} and \code{A -> mY} could parse) error and suggest the list
#'   form.
#' @param nodes Optional character vector of children to rebuild; default all
#'   endogenous nodes with parents.
#' @param quiet Logical. If \code{FALSE} (default), message kept vs saturated
#'   type counts.
#'
#' @return The model with replaced \code{nodal_types} and \code{parameters_df}.
#'   Cached \code{P}, \code{causal_types}, and \code{parmap} are cleared.
#'
#' @export
#' @examples
#' # Prefer list form: Y increasing in A, no QI in B, ...
#' m <- simplify_model(
#'   make_model("A -> Y <- B"),
#'   monotone = list(Y = c(A = "+", B = "m"))
#' )
#' # AND and OR survive "+"/ "m"; XNOR/XOR do not survive "m"
#' all(c("0001", "0111") %in% m$nodal_types$Y)
#' !any(c("1001", "0110") %in% m$nodal_types$Y)
#'
#' # Across the board: exclude qualitative interactions on every edge
#' m2 <- make_model("A -> Y <- B; C -> Y", monotone = "m")
#'
#' # Across the board: weakly increasing in every parent
#' m3 <- make_model(
#'   "A -> Y <- B; C -> Y",
#'   drop_interactions = TRUE,
#'   monotone = "+"
#' )
#' length(m3$nodal_types$Y)
#'
#' # Compact strings are optional (same as list(Y = c(A = "m")))
#' m4 <- simplify_model(make_model("A -> Y <- B"), monotone = "AmY")
#'
#' # Alias
#' identical(
#'   simplify_model(make_model("X -> Y"), monotone = "+")$nodal_types,
#'   set_nodal_restrictions(make_model("X -> Y"), monotone = "+")$nodal_types
#' )
simplify_model <- function(model,
                           drop_interactions = NULL,
                           keep_interactions = NULL,
                           monotone = NULL,
                           nodes = NULL,
                           quiet = FALSE) {
  is_a_model(model)
  if (any(model$parameters_df$given != "")) {
    stop(
      "simplify_model: model has confounding strata on parameters_df. ",
      "Apply type reductions before set_confound (or rebuild with make_model).",
      call. = FALSE
    )
  }

  specs <- normalize_nodal_restriction_args(
    model,
    drop_interactions = drop_interactions,
    keep_interactions = keep_interactions,
    monotone = monotone
  )
  if (!specs$active) {
    if (!quiet) {
      message("simplify_model: no drop_interactions or monotone restrictions; model unchanged.")
    }
    return(model)
  }

  parents <- get_parents(model)
  target <- if (is.null(nodes)) {
    model$nodes
  } else {
    intersect(model$nodes, nodes)
  }
  if (!length(target)) {
    stop("`nodes` did not match any model nodes.", call. = FALSE)
  }

  nt <- get_nodal_types(model, collapse = TRUE)
  for (v in target) {
    pa <- parents[[v]]
    if (!length(pa)) {
      next
    }
    before <- length(nt[[v]])
    nt[[v]] <- generate_restricted_nodal_types(
      parents = pa,
      drop_min_order = specs$drop_min_order,
      keep_sets = specs$keep_for_node(v),
      mono = specs$mono_for_node(v)
    )
    after <- length(nt[[v]])
    if (!after) {
      stop(
        "No nodal types remain for node ", v,
        " under the given restrictions.",
        call. = FALSE
      )
    }
    if (!quiet) {
      sat <- if (length(pa) <= .MAX_PARENTS_SAT) {
        2^(2^length(pa))
      } else {
        NA_real_
      }
      if (is.finite(sat)) {
        message(v, ": kept ", after, " types (saturated would be ",
                format(sat, big.mark = ","), ")")
      } else {
        message(v, ": kept ", after, " types (saturated set not enumerated)")
      }
    }
  }

  model$nodal_types <- nt
  attr(model$nodal_types, "interpret") <- interpret_type(model)
  model$parameters_df <- make_parameters_df(nt)
  model$P <- NULL
  model$causal_types <- NULL
  model$parmap <- NULL
  model$A <- NULL
  clear_model_cache(model)
}

#' @rdname simplify_model
#' @export
set_nodal_restrictions <- simplify_model


# ---- Argument normalization -------------------------------------------------

#' @keywords internal
#' @noRd
normalize_nodal_restriction_args <- function(model,
                                             drop_interactions,
                                             keep_interactions,
                                             monotone) {
  drop_min <- parse_drop_interactions(drop_interactions)
  mono_map <- parse_monotone(model, monotone)
  keep_map <- parse_keep_interactions(model, keep_interactions)

  active <- !is.null(drop_min) || length(mono_map) > 0L
  list(
    active = active,
    drop_min_order = drop_min,
    keep_for_node = function(v) {
      if (!is.null(keep_map[[v]])) {
        return(keep_map[[v]])
      }
      if (!is.null(keep_map[["*"]])) {
        return(keep_map[["*"]])
      }
      list()
    },
    mono_for_node = function(v) {
      if (is.null(mono_map[[v]])) {
        return(character(0))
      }
      mono_map[[v]]
    }
  )
}

#' @keywords internal
#' @noRd
parse_drop_interactions <- function(drop_interactions) {
  if (is.null(drop_interactions) || isFALSE(drop_interactions)) {
    return(NULL)
  }
  if (isTRUE(drop_interactions)) {
    return(2L)
  }
  if (is.character(drop_interactions)) {
    if (length(drop_interactions) == 1L &&
        tolower(drop_interactions) %in% c("all", "true")) {
      return(2L)
    }
    stop("`drop_interactions` character value must be \"all\".", call. = FALSE)
  }
  if (is.numeric(drop_interactions)) {
    x <- as.integer(drop_interactions)
    if (!length(x) || anyNA(x) || any(x < 2L) || any(x > 4L)) {
      stop("`drop_interactions` numeric values must be in 2:4.", call. = FALSE)
    }
    return(min(x))
  }
  stop("`drop_interactions` not recognized.", call. = FALSE)
}

#' @keywords internal
#' @noRd
parse_keep_interactions <- function(model, keep_interactions) {
  if (is.null(keep_interactions)) {
    return(list())
  }
  if (!is.list(keep_interactions)) {
    stop("`keep_interactions` must be a list of parent-name sets.", call. = FALSE)
  }
  # Unnamed list of sets → apply to all children
  if (is.null(names(keep_interactions)) || all(!nzchar(names(keep_interactions)))) {
    sets <- lapply(keep_interactions, function(s) {
      s <- as.character(s)
      if (!length(s)) {
        stop("Empty set in keep_interactions.", call. = FALSE)
      }
      s
    })
    return(list("*" = sets))
  }
  out <- list()
  for (nm in names(keep_interactions)) {
    if (!nm %in% model$nodes) {
      stop("keep_interactions name `", nm, "` is not a model node.", call. = FALSE)
    }
    val <- keep_interactions[[nm]]
    if (is.character(val)) {
      val <- list(val)
    }
    if (!is.list(val)) {
      stop("keep_interactions[[", nm, "]] must be a list of parent sets.", call. = FALSE)
    }
    out[[nm]] <- lapply(val, as.character)
  }
  out
}

#' @keywords internal
#' @noRd
.mono_codes <- c("+", "-", "m", "n")

#' Collect deltas for raising parent j under all backgrounds.
#' @keywords internal
#' @noRd
parent_effect_deltas <- function(f, parents, j) {
  k <- length(parents)
  others <- setdiff(seq_len(k), j)
  if (length(others) == 0L) {
    fixings <- matrix(integer(0), nrow = 1L, ncol = 0L)
  } else {
    fixings <- as.matrix(perm(rep(1, length(others))))
  }
  deltas <- integer(nrow(fixings))
  for (r in seq_len(nrow(fixings))) {
    bg <- integer(k)
    if (length(others)) {
      bg[others] <- as.integer(fixings[r, ])
    }
    bg[j] <- 0L
    i0 <- assignment_index(bg, parents)
    bg[j] <- 1L
    i1 <- assignment_index(bg, parents)
    deltas[[r]] <- f[i1] - f[i0]
  }
  deltas
}

#' @keywords internal
#' @noRd
parse_monotone <- function(model, monotone) {
  if (is.null(monotone)) {
    return(list())
  }
  parents <- get_parents(model)
  out <- list()

  add_edge <- function(parent, child, sign) {
    if (!child %in% model$nodes) {
      stop("Monotone child `", child, "` is not in the model.", call. = FALSE)
    }
    if (!parent %in% parents[[child]]) {
      stop(
        "Monotone: `", parent, "` is not a parent of `", child, "`.",
        call. = FALSE
      )
    }
    if (!sign %in% .mono_codes) {
      stop(
        "Monotone code must be one of '+', '-', 'm', 'n' (got `", sign, "`).",
        call. = FALSE
      )
    }
    cur <- out[[child]]
    if (is.null(cur)) {
      cur <- setNames(character(0), character(0))
    }
    cur[parent] <- sign
    out[[child]] <<- cur
  }

  if (is.character(monotone) && length(monotone) == 1L &&
      monotone %in% .mono_codes) {
    for (v in model$nodes) {
      for (p in parents[[v]]) {
        add_edge(p, v, monotone)
      }
    }
    return(out)
  }

  if (is.list(monotone) && !is.null(names(monotone))) {
    for (child in names(monotone)) {
      spec <- monotone[[child]]
      if (is.null(names(spec))) {
        stop(
          "list monotone for node ", child,
          " must be named with parent codes, e.g. c(A = 'm', B = '+').",
          call. = FALSE
        )
      }
      for (p in names(spec)) {
        add_edge(p, child, as.character(spec[[p]]))
      }
    }
    return(out)
  }

  if (is.character(monotone)) {
    for (spec in monotone) {
      spec <- gsub(" ", "", spec)
      if (spec %in% .mono_codes) {
        stop(
          "Global monotone code `", spec,
          "` must be a length-1 character, not mixed with edge specs.",
          call. = FALSE
        )
      }
      cands <- monotone_spec_candidates(spec, model, parents)
      if (!length(cands)) {
        stop(
          "Cannot parse monotone spec `", spec,
          "`. Prefer list(Y = c(A = 'm')). Compact forms: `A+Y`, `AmY`.",
          call. = FALSE
        )
      }
      if (length(cands) > 1L) {
        stop(
          "Monotone spec `", spec, "` is ambiguous. ",
          "Use list form, e.g. list(Y = c(A = 'm')).",
          call. = FALSE
        )
      }
      hit <- cands[[1]]
      if (identical(hit$kind, "edge")) {
        add_edge(hit$parent, hit$child, hit$sign)
      } else {
        # short form: both sides are parents of a unique shared child
        add_edge(hit$left, hit$child, hit$sign)
        add_edge(hit$right, hit$child, hit$sign)
      }
    }
    return(out)
  }

  stop("`monotone` not recognized.", call. = FALSE)
}

#' Possible parses of a compact monotone string against the DAG.
#' @keywords internal
#' @noRd
monotone_spec_candidates <- function(spec, model, parents) {
  chars <- strsplit(spec, "", fixed = TRUE)[[1]]
  out <- list()
  for (i in seq_along(chars)) {
    sgn <- chars[[i]]
    if (!sgn %in% .mono_codes) {
      next
    }
    left <- if (i > 1L) paste(chars[seq_len(i - 1L)], collapse = "") else ""
    right <- if (i < length(chars)) {
      paste(chars[seq.int(i + 1L, length(chars))], collapse = "")
    } else {
      ""
    }
    if (!nzchar(left) || !nzchar(right)) {
      next
    }
    # Edge: left -> right with code sgn
    if (right %in% model$nodes && left %in% parents[[right]]) {
      out[[length(out) + 1L]] <- list(
        kind = "edge", parent = left, child = right, sign = sgn
      )
    }
    # Short: left and right both parents of a unique common child
    children_of_both <- model$nodes[
      vapply(model$nodes, function(ch) {
        all(c(left, right) %in% parents[[ch]])
      }, logical(1))
    ]
    if (length(children_of_both) == 1L) {
      out[[length(out) + 1L]] <- list(
        kind = "pair",
        left = left,
        right = right,
        child = children_of_both[[1]],
        sign = sgn
      )
    }
  }
  # Deduplicate identical edge parses
  if (!length(out)) {
    return(out)
  }
  keys <- vapply(out, function(x) {
    if (identical(x$kind, "edge")) {
      paste("e", x$parent, x$sign, x$child, sep = "\r")
    } else {
      paste("p", x$left, x$right, x$sign, x$child, sep = "\r")
    }
  }, character(1))
  out[!duplicated(keys)]
}

#' Monotone / QI check for named parents.
#' @keywords internal
#' @noRd
type_respects_monotone <- function(type_string, parents, mono_named) {
  if (!length(mono_named)) {
    return(TRUE)
  }
  f <- type_string_to_f(type_string)
  for (p in names(mono_named)) {
    j <- match(p, parents)
    if (is.na(j)) {
      next
    }
    sign <- mono_named[[p]]
    deltas <- parent_effect_deltas(f, parents, j)
    if (sign == "+" && any(deltas < 0)) {
      return(FALSE)
    }
    if (sign == "-" && any(deltas > 0)) {
      return(FALSE)
    }
    # m: no qualitative interaction (no sign change across backgrounds)
    if (sign == "m" && any(deltas > 0) && any(deltas < 0)) {
      return(FALSE)
    }
    # n: keep only qualitative-interaction (sign-changing) types
    if (sign == "n" && !(any(deltas > 0) && any(deltas < 0))) {
      return(FALSE)
    }
  }
  TRUE
}


# ---- Type generation --------------------------------------------------------

#' Parent assignment grid matching type_matrix / realise_outcomes order.
#' @keywords internal
#' @noRd
parent_assignment_grid <- function(parents) {
  k <- length(parents)
  if (k == 0L) {
    return(matrix(integer(0), nrow = 1L, ncol = 0L))
  }
  g <- as.matrix(perm(rep(1, k)))
  colnames(g) <- parents
  g
}

# Caps for generative / saturated nodal-type construction
.MAX_PARENTS_SAT <- 4L
.MAX_PARENTS_STRING <- 16L
.MAX_KEEP_BLOCK <- 4L
.MAX_NODAL_TYPES_NODE <- 65536L
# Max ANOVA dimension for free-cell enum (2^d candidates tried)
.MAX_ORDER_LEQ_DIM <- 16L

#' All saturated collapsed nodal types for a parent set (k <= 4).
#' @keywords internal
#' @noRd
saturated_nodal_types <- function(parents) {
  k <- length(parents)
  if (k == 0L) {
    return(c("0", "1"))
  }
  if (k > .MAX_PARENTS_SAT) {
    stop(
      "Cannot enumerate saturated nodal types for ", k,
      " parents; supply `nodal_types` or use `drop_interactions = TRUE` ",
      "(optionally with `monotone` / `keep_interactions`) so types can be ",
      "built without materialising the saturated set.",
      call. = FALSE
    )
  }
  mat <- type_matrix(k)
  apply(mat, 1L, paste, collapse = "")
}

#' Response vector from collapsed type string.
#' @keywords internal
#' @noRd
type_string_to_f <- function(type_string) {
  as.integer(strsplit(type_string, "", fixed = TRUE)[[1]])
}

#' Index of assignment row (1-based) for parent values in grid order.
#' @keywords internal
#' @noRd
assignment_index <- function(values, parents) {
  # values named or in parents order; bit packing matches realise_outcomes
  if (is.null(names(values))) {
    pv <- as.integer(values)
  } else {
    pv <- as.integer(values[parents])
  }
  as.integer(sum(vapply(seq_along(pv), function(j) {
    bitwShiftL(1L, as.integer(j - 1L)) * pv[[j]]
  }, integer(1)))) + 1L
}

#' Whether f has a 2-way interaction on (j, ell) under exists-fixing rule.
#' @keywords internal
#' @noRd
has_pairwise_interaction <- function(f, parents, j, ell) {
  k <- length(parents)
  others <- setdiff(seq_len(k), c(j, ell))
  # All fixings of others
  if (length(others) == 0L) {
    fixings <- matrix(integer(0), nrow = 1L, ncol = 0L)
  } else {
    fixings <- as.matrix(perm(rep(1, length(others))))
  }
  for (r in seq_len(nrow(fixings))) {
    bg <- integer(k)
    if (length(others)) {
      bg[others] <- as.integer(fixings[r, ])
    }
    # Delta_j at ell = 0 and ell = 1
    d <- c(NA_integer_, NA_integer_)
    for (ell_val in 0:1) {
      bg[ell] <- ell_val
      bg[j] <- 0L
      i0 <- assignment_index(bg, parents)
      bg[j] <- 1L
      i1 <- assignment_index(bg, parents)
      d[ell_val + 1L] <- f[i1] - f[i0]
    }
    if (d[1] != d[2]) {
      return(TRUE)
    }
  }
  FALSE
}

#' Order-3: 2-way (j,ell) contrast depends on m (exists fixing).
#' @keywords internal
#' @noRd
has_three_way_interaction <- function(f, parents, j, ell, m) {
  k <- length(parents)
  others <- setdiff(seq_len(k), c(j, ell, m))
  if (length(others) == 0L) {
    fixings <- matrix(integer(0), nrow = 1L, ncol = 0L)
  } else {
    fixings <- as.matrix(perm(rep(1, length(others))))
  }
  for (r in seq_len(nrow(fixings))) {
    bg <- integer(k)
    if (length(others)) {
      bg[others] <- as.integer(fixings[r, ])
    }
    twoway <- c(NA_integer_, NA_integer_)
    for (m_val in 0:1) {
      bg[m] <- m_val
      d <- c(NA_integer_, NA_integer_)
      for (ell_val in 0:1) {
        bg[ell] <- ell_val
        bg[j] <- 0L
        i0 <- assignment_index(bg, parents)
        bg[j] <- 1L
        i1 <- assignment_index(bg, parents)
        d[ell_val + 1L] <- f[i1] - f[i0]
      }
      twoway[m_val + 1L] <- d[2] - d[1]
    }
    if (twoway[1] != twoway[2]) {
      return(TRUE)
    }
  }
  FALSE
}

#' Order-4: 3-way contrast depends on a fourth index.
#' @keywords internal
#' @noRd
has_four_way_interaction <- function(f, parents, idxs) {
  # idxs length 4: j, ell, m, n
  j <- idxs[[1]]
  ell <- idxs[[2]]
  m <- idxs[[3]]
  n <- idxs[[4]]
  k <- length(parents)
  others <- setdiff(seq_len(k), idxs)
  if (length(others) == 0L) {
    fixings <- matrix(integer(0), nrow = 1L, ncol = 0L)
  } else {
    fixings <- as.matrix(perm(rep(1, length(others))))
  }
  for (r in seq_len(nrow(fixings))) {
    bg <- integer(k)
    if (length(others)) {
      bg[others] <- as.integer(fixings[r, ])
    }
    three <- c(NA_integer_, NA_integer_)
    for (n_val in 0:1) {
      bg[n] <- n_val
      twoway <- c(NA_integer_, NA_integer_)
      for (m_val in 0:1) {
        bg[m] <- m_val
        d <- c(NA_integer_, NA_integer_)
        for (ell_val in 0:1) {
          bg[ell] <- ell_val
          bg[j] <- 0L
          i0 <- assignment_index(bg, parents)
          bg[j] <- 1L
          i1 <- assignment_index(bg, parents)
          d[ell_val + 1L] <- f[i1] - f[i0]
        }
        twoway[m_val + 1L] <- d[2] - d[1]
      }
      three[n_val + 1L] <- twoway[2] - twoway[1]
    }
    if (three[1] != three[2]) {
      return(TRUE)
    }
  }
  FALSE
}

#' Interaction on parent index set allowed by keep_sets?
#' @keywords internal
#' @noRd
interaction_kept <- function(parent_names, keep_sets) {
  if (!length(keep_sets)) {
    return(FALSE)
  }
  for (ks in keep_sets) {
    if (all(parent_names %in% ks)) {
      return(TRUE)
    }
  }
  FALSE
}

#' Does type violate drop_interactions (min order)?
#' @keywords internal
#' @noRd
type_has_forbidden_interaction <- function(type_string,
                                           parents,
                                           drop_min_order,
                                           keep_sets) {
  if (is.null(drop_min_order)) {
    return(FALSE)
  }
  k <- length(parents)
  if (k < drop_min_order) {
    return(FALSE)
  }
  f <- type_string_to_f(type_string)
  # Order 2
  if (drop_min_order <= 2L && k >= 2L) {
    for (j in seq_len(k - 1L)) {
      for (ell in seq.int(j + 1L, k)) {
        if (has_pairwise_interaction(f, parents, j, ell)) {
          pn <- parents[c(j, ell)]
          if (!interaction_kept(pn, keep_sets)) {
            return(TRUE)
          }
        }
      }
    }
  }
  # Order 3
  if (drop_min_order <= 3L && k >= 3L) {
    for (j in seq_len(k - 2L)) {
      for (ell in seq.int(j + 1L, k - 1L)) {
        for (m in seq.int(ell + 1L, k)) {
          if (has_three_way_interaction(f, parents, j, ell, m)) {
            pn <- parents[c(j, ell, m)]
            if (!interaction_kept(pn, keep_sets)) {
              return(TRUE)
            }
          }
        }
      }
    }
  }
  # Order 4
  if (drop_min_order <= 4L && k >= 4L) {
    combos <- utils::combn(k, 4L)
    for (c in seq_len(ncol(combos))) {
      idxs <- combos[, c]
      if (has_four_way_interaction(f, parents, idxs)) {
        pn <- parents[idxs]
        if (!interaction_kept(pn, keep_sets)) {
          return(TRUE)
        }
      }
    }
  }
  FALSE
}

#' Regime for restricted nodal-type generation (never saturates when avoidable).
#' @return "root", "unary", "blocks", "order_leq", "sat", or "refuse"
#' @keywords internal
#' @noRd
restriction_regime <- function(k, drop_min_order, keep_sets) {
  if (k == 0L) {
    return("root")
  }
  if (is.null(drop_min_order)) {
    if (k <= .MAX_PARENTS_SAT) {
      return("sat")
    }
    return("refuse")
  }
  r <- as.integer(drop_min_order) - 1L
  if (length(keep_sets)) {
    # keep exceptions: lean block union only for drop-order >= 2 (r <= 1);
    # higher-order drop with keeps still needs saturate+filter for k <= 4
    if (r <= 1L) {
      return("blocks")
    }
    if (k <= .MAX_PARENTS_SAT) {
      return("sat")
    }
    return("refuse")
  }
  if (r <= 1L) {
    return("unary")
  }
  if (r >= k) {
    if (k <= .MAX_PARENTS_SAT) {
      return("sat")
    }
    return("refuse")
  }
  # order <= r via free-cell enumeration in the ANOVA subspace
  d <- anova_order_dim(k, r)
  if (is.finite(d) && d <= .MAX_ORDER_LEQ_DIM && 2^d <= .MAX_NODAL_TYPES_NODE) {
    return("order_leq")
  }
  if (k <= .MAX_PARENTS_SAT) {
    return("sat")
  }
  "refuse"
}

#' Dimension of real functions on {0,1}^k with interaction order <= r.
#' @keywords internal
#' @noRd
anova_order_dim <- function(k, r) {
  r <- min(as.integer(r), as.integer(k))
  if (r < 0L) {
    return(0)
  }
  sum(vapply(0:r, function(i) choose(k, i), numeric(1)))
}

#' Monomial design matrix for interaction order <= r (parent grid row order).
#' @keywords internal
#' @noRd
monomial_design_order_leq <- function(grid, r) {
  k <- ncol(grid)
  n <- nrow(grid)
  subsets <- list(integer(0))
  if (r >= 1L) {
    for (s in seq_len(min(r, k))) {
      subsets <- c(subsets, utils::combn(k, s, simplify = FALSE))
    }
  }
  d <- length(subsets)
  M <- matrix(1, nrow = n, ncol = d)
  for (j in seq_along(subsets)) {
    S <- subsets[[j]]
    if (length(S)) {
      M[, j] <- apply(grid[, S, drop = FALSE], 1L, prod)
    }
  }
  list(M = M, subsets = subsets)
}

#' Binary vectors of length d as rows (bit 0 = least significant).
#' @keywords internal
#' @noRd
binary_enum_matrix <- function(d) {
  n <- 2^d
  out <- matrix(0L, nrow = n, ncol = d)
  if (d == 0L) {
    return(out)
  }
  for (j in seq_len(d)) {
    period <- 2^(j - 1L)
    out[, j] <- rep(rep(c(0L, 1L), each = period), length.out = n)
  }
  out
}

#' Generate nodal types with ANOVA interaction order at most r.
#'
#' Uses free cells at Hamming weight <= r (same parent-assignment order as
#' \code{parent_assignment_grid}) and solves the degree-<=r monomial system.
#' Does not change node or parent ordering.
#'
#' @keywords internal
#' @noRd
generate_order_leq_nodal_types <- function(parents, r) {
  k <- length(parents)
  r <- as.integer(r)
  if (r <= 1L) {
    return(generate_unary_nodal_types(parents))
  }
  if (r >= k) {
    return(saturated_nodal_types(parents))
  }
  grid <- parent_assignment_grid(parents)
  des <- monomial_design_order_leq(grid, r)
  M <- des$M
  free <- which(rowSums(grid) <= r)
  if (length(free) != ncol(M)) {
    stop(
      "Internal error: free-cell count (", length(free),
      ") != ANOVA dimension (", ncol(M), ").",
      call. = FALSE
    )
  }
  M_free <- M[free, , drop = FALSE]
  if (qr(M_free)$rank < ncol(M)) {
    stop(
      "Internal error: order-<=", r, " free-cell design is rank-deficient.",
      call. = FALSE
    )
  }
  Minv <- solve(M_free)
  d <- ncol(M)
  bits <- binary_enum_matrix(d)
  n_enum <- nrow(bits)
  out <- character(n_enum)
  n_keep <- 0L
  for (i in seq_len(n_enum)) {
    b <- bits[i, ]
    beta <- as.vector(Minv %*% b)
    f <- as.vector(M %*% beta)
    if (max(abs(f - round(f))) > 1e-7) {
      next
    }
    f01 <- as.integer(round(f))
    if (any(f01 < 0L | f01 > 1L)) {
      next
    }
    if (!all(f01[free] == b)) {
      next
    }
    n_keep <- n_keep + 1L
    out[[n_keep]] <- paste(f01, collapse = "")
  }
  unique(out[seq_len(n_keep)])
}

#' Upper bound on kept nodal types under restrictions (no allocation).
#' @keywords internal
#' @noRd
estimate_restricted_nodal_type_count <- function(parents,
                                                 drop_min_order = NULL,
                                                 keep_sets = list(),
                                                 mono = character(0)) {
  k <- length(parents)
  regime <- restriction_regime(k, drop_min_order, keep_sets)
  if (identical(regime, "root")) {
    return(2)
  }
  if (identical(regime, "refuse")) {
    return(Inf)
  }
  if (identical(regime, "sat")) {
    return(as.numeric(2^(2^k)))
  }
  if (identical(regime, "order_leq")) {
    r <- as.integer(drop_min_order) - 1L
    return(as.numeric(2^anova_order_dim(k, r)))
  }
  # QI codes cannot survive unary / block schedules that ignore other parents
  if (length(mono) && any(mono == "n", na.rm = TRUE)) {
    # blocks may still admit QI inside a kept pair; unary never does
    if (identical(regime, "unary")) {
      return(0)
    }
  }
  if (identical(regime, "unary")) {
    # constants + each parent and its negation; mono may drop half
    ub <- 2L + 2L * k
    if (length(mono)) {
      signs <- unname(mono[names(mono) %in% parents])
      if (length(signs) && all(signs %in% c("+", "-"))) {
        # each constrained parent keeps one orientation; unconstrained keep both
        n_con <- sum(parents %in% names(mono))
        ub <- 2L + n_con + 2L * (k - n_con)
      }
    }
    return(as.numeric(ub))
  }
  # blocks: union of functions of each keep-set and each singleton
  if (any(vapply(keep_sets, length, integer(1)) > .MAX_KEEP_BLOCK)) {
    return(Inf)
  }
  ub <- 0
  for (ks in keep_sets) {
    ks <- intersect(ks, parents)
    if (!length(ks)) {
      next
    }
    ub <- ub + 2^(2^length(ks))
  }
  ub <- ub + 2 + 2 * k
  as.numeric(ub)
}

#' Schedules that depend only on parents in S (constant in others).
#' @keywords internal
#' @noRd
nodal_types_depending_only_on <- function(parents, S) {
  k <- length(parents)
  n_assign <- 2^k
  if (!length(S)) {
    return(c(
      paste(rep(0L, n_assign), collapse = ""),
      paste(rep(1L, n_assign), collapse = "")
    ))
  }
  S <- intersect(S, parents)
  if (!length(S)) {
    return(nodal_types_depending_only_on(parents, character(0)))
  }
  if (length(S) > .MAX_KEEP_BLOCK) {
    stop(
      "keep_interactions blocks of size > ", .MAX_KEEP_BLOCK,
      " cannot be enumerated.",
      call. = FALSE
    )
  }
  grid <- parent_assignment_grid(parents)
  mats <- type_matrix(length(S))
  sub_bits <- grid[, match(S, parents), drop = FALSE]
  sub_index <- apply(sub_bits, 1L, function(row) {
    assignment_index(as.integer(row), S)
  })
  apply(mats, 1L, function(g) paste(g[sub_index], collapse = ""))
}

#' Unary (no pairwise interaction) candidate schedules.
#' @keywords internal
#' @noRd
generate_unary_nodal_types <- function(parents) {
  k <- length(parents)
  n_assign <- 2^k
  grid <- parent_assignment_grid(parents)
  candidates <- c(
    paste(rep(0L, n_assign), collapse = ""),
    paste(rep(1L, n_assign), collapse = "")
  )
  for (j in seq_len(k)) {
    candidates <- c(
      candidates,
      paste(grid[, j], collapse = ""),
      paste(1L - grid[, j], collapse = "")
    )
  }
  unique(candidates)
}

#' Block candidates: functions of each keep-set plus unary schedules.
#' @keywords internal
#' @noRd
generate_block_nodal_types <- function(parents, keep_sets) {
  candidates <- generate_unary_nodal_types(parents)
  for (ks in keep_sets) {
    ks <- intersect(as.character(ks), parents)
    if (length(ks) >= 2L) {
      candidates <- c(candidates, nodal_types_depending_only_on(parents, ks))
    }
  }
  unique(candidates)
}

#' Generate allowed collapsed nodal types for one node.
#'
#' Uses a lean generative path when \code{drop_interactions} is set: order
#' \eqn{\le 1} via unary / keep-block schedules, higher max-order via free-cell
#' enumeration in the ANOVA subspace (never materialising
#' \eqn{2^{2^k}} unless the saturated regime is required for \eqn{k \le 4}).
#'
#' @keywords internal
#' @noRd
generate_restricted_nodal_types <- function(parents,
                                            drop_min_order = NULL,
                                            keep_sets = list(),
                                            mono = character(0)) {
  k <- length(parents)
  if (k == 0L) {
    return(c("0", "1"))
  }
  if (k > .MAX_PARENTS_STRING) {
    stop(
      "Automatic type reduction supports at most ", .MAX_PARENTS_STRING,
      " parents per node (node has ", k, ") because each type string has ",
      "length 2^k. Pass `nodal_types` explicitly or reduce parents.",
      call. = FALSE
    )
  }

  regime <- restriction_regime(k, drop_min_order, keep_sets)
  if (identical(regime, "refuse")) {
    stop(
      "Cannot auto-build nodal types for ", k, " parents without ",
      "`drop_interactions` that drops order >= 2 (saturated size is 2^(2^",
      k, ")). Pass `drop_interactions = TRUE` and/or explicit `nodal_types`.",
      call. = FALSE
    )
  }

  n_hat <- estimate_restricted_nodal_type_count(
    parents, drop_min_order, keep_sets, mono
  )
  if (!is.finite(n_hat) || n_hat > .MAX_NODAL_TYPES_NODE) {
    stop(
      "Estimated nodal types for this node exceed ",
      format(.MAX_NODAL_TYPES_NODE, big.mark = ","),
      " (estimate ", format(n_hat, big.mark = ","), "). ",
      "Tighten `drop_interactions` / `keep_interactions` or pass `nodal_types`.",
      call. = FALSE
    )
  }

  candidates <- switch(
    regime,
    unary = generate_unary_nodal_types(parents),
    blocks = generate_block_nodal_types(parents, keep_sets),
    order_leq = generate_order_leq_nodal_types(
      parents, as.integer(drop_min_order) - 1L
    ),
    sat = saturated_nodal_types(parents),
    stop("Unknown restriction regime.", call. = FALSE)
  )

  # Generative regimes already obey interaction-order rules; only sat needs
  # the interaction filter. Monotone always applied.
  if (regime %in% c("unary", "blocks", "order_leq")) {
    keep <- vapply(
      candidates,
      function(ts) type_respects_monotone(ts, parents, mono),
      logical(1)
    )
  } else {
    keep <- vapply(candidates, function(ts) {
      if (type_has_forbidden_interaction(ts, parents, drop_min_order, keep_sets)) {
        return(FALSE)
      }
      type_respects_monotone(ts, parents, mono)
    }, logical(1))
  }
  unname(candidates[keep])
}

#' Apply make_model type-restriction args → nodal_types list for all nodes.
#' @keywords internal
#' @noRd
build_nodal_types_with_restrictions <- function(model,
                                                drop_interactions = NULL,
                                                keep_interactions = NULL,
                                                monotone = NULL,
                                                quiet = FALSE) {
  specs <- normalize_nodal_restriction_args(
    model,
    drop_interactions = drop_interactions,
    keep_interactions = keep_interactions,
    monotone = monotone
  )
  parents <- get_parents(model)
  nt <- vector("list", length(model$nodes))
  names(nt) <- model$nodes
  for (v in model$nodes) {
    pa <- parents[[v]]
    if (!length(pa)) {
      nt[[v]] <- c("0", "1")
      next
    }
    if (!specs$active) {
      nt[[v]] <- saturated_nodal_types(pa)
    } else {
      nt[[v]] <- generate_restricted_nodal_types(
        parents = pa,
        drop_min_order = specs$drop_min_order,
        keep_sets = specs$keep_for_node(v),
        mono = specs$mono_for_node(v)
      )
      if (!length(nt[[v]])) {
        stop("No nodal types remain for node ", v, ".", call. = FALSE)
      }
      if (!quiet) {
        sat <- if (length(pa) <= .MAX_PARENTS_SAT) {
          2^(2^length(pa))
        } else {
          NA_real_
        }
        if (is.finite(sat)) {
          message(v, ": kept ", length(nt[[v]]), " types (saturated would be ",
                  format(sat, big.mark = ","), ")")
        } else {
          message(v, ": kept ", length(nt[[v]]),
                  " types (saturated set not enumerated)")
        }
      }
    }
  }
  nt
}
