#' Make a model
#'
#' \code{make_model} uses causal statements encoded as strings to specify
#' the nodes and edges of a graph. Implied nodal types are calculated
#' and default priors are provided under the assumption of no confounding.
#' Models can be updated with restrictions on types and/or informative
#' priors on parameters.
#'
#' @section Large and many-parent models:
#' The saturated type space grows very fast: a node with \(k\) binary parents
#' has \(2^{2^k}\) nodal types (2, 4, 16, 256, 65536 for \(k = 1,\ldots,4\)).
#' Practical options:
#' \itemize{
#'   \item Use the default \code{legacy = FALSE} path so a global causal-type
#'     table is not attached at build.
#'   \item Pass \code{drop_interactions} and/or \code{monotone} to build a
#'     reduced type set at construction (see \code{\link{simplify_model}}),
#'     e.g. \code{drop_interactions = TRUE, monotone = "+"}. With interaction
#'     dropping, more than four parents are allowed because types are
#'     generated without materialising the saturated set.
#'   \item Pass a short \code{nodal_types} list for busy nodes (required for
#'     five or more parents unless interaction dropping applies).
#'   \item On an existing model, call \code{\link{simplify_model}}.
#'   \item Set \code{allow_large = TRUE} only if you intentionally exceed
#'     the soft limits on causal-type product or confound-expanded
#'     parameters.
#' }
#' Nodal-type strings are one digit per parent assignment. Assignment order
#' matches \code{get_parents()} / the column order in the node's type matrix.
#'
#' @param statement character string. Statement describing causal
#'   relations between nodes. Directed relations can be specified
#'   using '->' or '<-' and can be combined.
#'   For instance "X -> Y", "Y <- X" or  "X1 -> Y <- X2; X1 -> X2".
#'   Confounded relations can be specified using a double headed arrow,
#'   "X <-> Y", to indicate unobserved confounding between X and Y.
#' @param add_causal_types Logical. Whether to create and attach causal
#'   types to \code{model}. Under \code{legacy = TRUE} defaults to `TRUE`.
#'   Under \code{legacy = FALSE} (default) causal types are not attached
#'   regardless of this argument.
#' @param nodal_types Named list of character vectors of nodal types for
#'   **every** node (same names and causal order as \code{model$nodes}).
#'   Use this to keep a many-parent node small. If \code{NULL}, types are
#'   auto-generated (refused for five or more parents on one node).
#' @param allow_large Logical. Soft limits apply to (i) the causal-type
#'   product when types are attached, and (ii) the parameter count after
#'   confound stratification (confounding multiplies parameters for the
#'   expanded node). When either exceeds one million, \code{make_model}
#'   errors unless \code{allow_large} is `TRUE` (then warns). Defaults
#'   to `FALSE`. Setting \code{add_causal_types = FALSE} or
#'   \code{legacy = FALSE} skips attaching the causal-type product but
#'   still applies the parameter-count guard. Restricted \code{nodal_types}
#'   (or \code{drop_interactions} / \code{monotone}) are assessed by their
#'   actual lengths.
#' @param legacy Logical. \code{FALSE} (default) is the new parameters-only /
#'   factorized path (no global causal-type table at build). \code{TRUE}
#'   restores the current causal-type expansion. Override with
#'   \code{options(CausalQueries.legacy = TRUE)}.
#' @param drop_interactions Drop interaction orders at least this high when
#'   auto-building nodal types. See \code{\link{simplify_model}}.
#' @param keep_interactions Interaction parent-sets to keep despite
#'   \code{drop_interactions}. See \code{\link{simplify_model}}.
#' @param monotone Monotonicity restrictions applied when building nodal
#'   types. See \code{\link{simplify_model}}.
#' @export
#'
#' @return An object of class \code{causal_model}.
#'
#' An object of class \code{"causal_model"} is a list containing at least the
#' following components:
#' \item{statement}{A character vector of the statement that defines the model}
#' \item{dag}{A \code{data.frame} with columns `parent`and `children`
#'   indicating how nodes relate to each other.}
#' \item{nodes}{A named \code{list} with the nodes in the model}
#' \item{parents_df}{A \code{data.frame} listing nodes, whether they are
#'   root nodes or not, and the number of parents they have}
#' \item{nodal_types}{Optional: A named \code{list} with the nodal types in
#'   the model. List should be ordered according to the causal ordering of
#'   nodes. If NULL nodal types are generated. If FALSE, a parameters data
#'   frame is not generated.}
#' \item{parameters_df}{A \code{data.frame} with descriptive information
#'   of the parameters in the model}
#' \item{causal_types}{A \code{data.frame} listing causal types and the
#'   nodal types that produce them (legacy / when attached)}
#'
#' By default a causal model has flat (uniform) priors and parameters that
#' put equal weight on each parameter within each parameter set. The parameter
#' ranges (range of the nodal types) can be adjusted with \code{\link{set_restrictions}}.
#' The priors can be adjusted with \code{\link{set_priors}}. Specific parameter
#' values can be adjusted with \code{\link{set_parameters}}.
#'
#' @seealso \code{\link{summary.causal_model}} provides summary method for
#'   output objects of class \code{causal_model}
#'
#' @references
#' Tietz T, Medina L, Syunyaev G, Humphreys M (2026).
#' "Making, Updating, and Querying Causal Models with CausalQueries."
#' \emph{Journal of Statistical Software}, \bold{117}(1), 1--40.
#' \doi{10.18637/jss.v117.i01}.
#'
#' @examples
#' make_model(statement = "X -> Y")
#' modelXKY <- make_model("X -> K -> Y; X -> Y")
#'
#' # Example where a cyclical dag is attempted
#' \dontrun{
#'  modelXKX <- make_model("X -> K -> X")
#' }
#'
#' # Examples with confounding
#' model <- make_model("X->Y; X <-> Y")
#' inspect(model, "parameter_matrix")
#' model <- make_model("Y2 <- X -> Y1; X <-> Y1; X <-> Y2")
#' dim(inspect(model, "parameter_matrix"))
#' inspect(model, "parameter_matrix")
#' model <- make_model("X1 -> Y <- X2; X1 <-> Y; X2 <-> Y")
#' dim(inspect(model, "parameter_matrix"))
#' inspect(model, "parameters_df")
#'
#' # A single node graph is also possible
#' model <- make_model("X")
#'
#' # Unconnected nodes not allowed
#' \dontrun{
#'  model <- make_model("X <-> Y")
#' }
#'
#' # ---- Large / many-parent models ------------------------------------------
#'
#' # Preferred: reduce types at construction (no 65k table for 4 parents)
#' m_bare <- make_model(
#'   "A -> Y <- B; C -> Y; D -> Y",
#'   drop_interactions = TRUE,
#'   monotone = "+",
#'   legacy = FALSE
#' )
#' length(m_bare$nodal_types$Y)
#' nrow(m_bare$parameters_df)
#'
#' # Default legacy = FALSE: wider DAG without attaching causal_types
#' big <- make_model("A -> M -> Y; B -> Y; C -> Y", legacy = FALSE)
#' big$causal_types  # NULL
#'
#' # Or thin after the fact
#' make_model("A -> Y <- B; C -> Y") |>
#'   simplify_model(drop_interactions = 2, monotone = "+") |>
#'   inspect("nodal_types")
#'
#' # Hand-specified types still work (e.g. five parents)
#' pa5 <- c("A", "B", "C", "D", "E")
#' assign5 <- as.matrix(expand.grid(lapply(pa5, function(p) 0:1)))[, pa5, drop = FALSE]
#' nt5 <- c(
#'   setNames(lapply(pa5, function(p) c("0", "1")), pa5),
#'   list(Y = c(
#'     paste(rep(0, nrow(assign5)), collapse = ""),
#'     paste(rep(1, nrow(assign5)), collapse = ""),
#'     paste(assign5[, 1], collapse = "")
#'   ))
#' )
#' make_model("A -> Y; B -> Y; C -> Y; D -> Y; E -> Y",
#'            nodal_types = nt5) |>
#'   inspect("parameters_df")
#'
#' make_model("Z -> Y", nodal_types = list(Z = c("0", "1"), Y = c("01", "10"))) |>
#'   inspect("parameters_df")
#'
#' \dontrun{
#' # Saturated four-parent Y (65,536 types) — avoid unless you mean it
#' make_model("A -> Y <- B; C -> Y; D -> Y")
#'
#' # Legacy path with a huge causal-type *product*: allow_large = TRUE
#' make_model("A -> Y <- B; C -> Y; D -> Y", legacy = TRUE, allow_large = TRUE)
#' }


make_model <- function(statement = "X -> Y",
                       add_causal_types = TRUE,
                       nodal_types = NULL,
                       allow_large = FALSE,
                       legacy = NULL,
                       drop_interactions = NULL,
                       keep_interactions = NULL,
                       monotone = NULL) {

  parent <- NULL
  legacy <- resolve_legacy(legacy)

  if (!is.character(statement) || length(statement) != 1) {
    stop("The model statement should be a single character string.")
  }

  if (!isTRUE(legacy)) {
    add_causal_types <- FALSE
  }

  restrict_args_set <- !(is.null(drop_interactions) || isFALSE(drop_interactions)) ||
    !is.null(keep_interactions) ||
    !is.null(monotone)
  if (restrict_args_set && !is.null(nodal_types)) {
    stop(
      "Pass either `nodal_types` or type-reduction arguments ",
      "(`drop_interactions` / `keep_interactions` / `monotone`), not both.",
      call. = FALSE
    )
  }


  # generate DAG
  .dag <- make_dag(statement)

  # clean dag statement
  statement <- ifelse(nrow(.dag) == 1 & all(is.na(.dag$e)),
                      statement,
                      paste(paste(.dag$v, .dag$e, .dag$w), collapse = "; "))

  # parent child data.frame
  dag  <- .dag |>
    dplyr::filter(e == "->" | is.na(e)) |>
    dplyr::select(v, w)

  # disallow dangling confound e.g. X -> M <-> Y (single nodes allowed)
  if (any(!(unlist(.dag[, 1:2]) %in% unlist(dag)))) {
    stop("Graph should not contain isolates.")
  }

  names(dag) <- c("parent", "children")

  # Procedure for unique ordering of nodes; ties broken by alphabet
  if (all(dag$parent %in% dag$children)) {
    stop("No root nodes provided.")
  }

  gen <- rep(NA, nrow(dag))
  j <- 1
  # assign 1 to exogenous nodes
  gen[!(dag$parent %in% dag$children)] <- j
  while (sum(is.na(gen)) > 0) {
    j <- j + 1
    xx <- (dag$parent %in% dag$children[is.na(gen)])
    if (all(xx[is.na(gen)])) {
      stop(paste("Cycling at generation", j))
    }
    gen[!xx & is.na(gen)] <- j
  }

  # dag is now given a causal order which is preserved in the parameters_df
  dag <- dag[order(gen, dag[, 1], dag[, 2]), ]

  endog_node <- as.character(rev(unique(rev(dag$children))))
  if (all(is.na(endog_node))) {
    endog_node <- NULL
  }
  .exog_node <- as.character(rev(unique(rev(dag$parent))))
  exog_node  <- .exog_node[!(.exog_node %in% endog_node)]

  # ordered nodes
  nodes <- c(exog_node, endog_node)

  # parent count df
   parents_df <-
     data.frame(node = nodes, root = nodes %in% exog_node) |>
     dplyr::mutate(parents = vapply(node, function(n) {
       dag |>
         dplyr::filter(children == n) |>
         nrow()
     }, numeric(1))) |>
     dplyr::mutate(parent_nodes = sapply(node, function(n) {
       dag |>
         dplyr::filter(children == n) |>
         dplyr::pull(parent) |>
         paste(collapse = ", ")
     }))

  # Model is a list
  model <-
    list(statement = statement,
         nodes = nodes,
         parents_df = parents_df,
         legacy = legacy)

  # Nodal types
  # Check nodal types map to nodes in model
  if ((!is.null(nodal_types)) &&
      (!all(names(nodal_types) %in% nodes))) {
    stop("Check provided nodal_types are nodes in the model")
  }

  # Check ordering and completeness
  if (!is.null(nodal_types) && !is.logical(nodal_types)) {
    if (!all(sort(names(nodal_types)) == sort(nodes))) {
      stop(
        paste(
          "Model not properly defined: If you provide nodal types you should",
          "do so for all nodes in model: ",
          paste(nodes, collapse = ", ")
        )
      )
    }

    if (!all(names(nodal_types) == nodes)) {
      message(paste(
        "Ordering of provided nodal types is being altered to",
        "match generation"
      ))
      nodal_types <- lapply(nodes, function(n)
        nodal_types[[n]])
      names(nodal_types) <- nodes
    }
  }

  if (is.logical(nodal_types)) {
    add_causal_types <- FALSE
    message(
      paste(
        "Model not properly defined: nodal_types should be NULL or specified",
        "for all nodes in model: ",
        paste(nodes, collapse = ", ")
      )
    )
  }

  # Confound plan (for complexity checks before set_confound expands params)
  confound_pairs <- if (grepl("<->", statement)) {
    confound_pairs_from_dag(.dag, nodes)
  } else {
    NULL
  }

  if (is.null(nodal_types)) {
    if (restrict_args_set) {
      nodal_types <- build_nodal_types_with_restrictions(
        model,
        drop_interactions = drop_interactions,
        keep_interactions = keep_interactions,
        monotone = monotone,
        quiet = FALSE
      )
      check_causal_type_count(lengths(nodal_types),
                              allow_large = allow_large,
                              add_causal_types = add_causal_types,
                              confound_pairs = confound_pairs)
    } else {
      n_types <- implied_nodal_type_counts(parents_df$parents)
      names(n_types) <- parents_df$node
      check_autogenerated_nodal_types(n_types)
      check_causal_type_count(n_types,
                              allow_large = allow_large,
                              add_causal_types = add_causal_types,
                              confound_pairs = confound_pairs)
      nodal_types <- get_nodal_types(model, collapse = TRUE)
    }
  } else if (!is.logical(nodal_types)) {
    check_causal_type_count(lengths(nodal_types),
                            allow_large = allow_large,
                            add_causal_types = add_causal_types,
                            confound_pairs = confound_pairs)
  }

  # Add nodal types to model
  model$nodal_types <- nodal_types

  # Add nodal type interpretation
  if (is.null(attr(nodal_types, "interpret"))) {
    attr(model$nodal_types, "interpret") <- interpret_type(model)
  }

  # Parameters data frame
  if (!is.logical(nodal_types)) {
    model$parameters_df <- make_parameters_df(nodal_types)
  }

  # Add class
  class(model) <- "causal_model"

  # Derived-object cache (realise_outcomes, etc.); cleared by mutators
  model <- ensure_model_cache(model)

  # Add causal types
  if (add_causal_types) {
    model$causal_types <- update_causal_types(model)
  }

  # Add confounds if any provided
  if (length(confound_pairs)) {
    if (any(!(c(names(confound_pairs), unlist(confound_pairs)) %in% nodes))) {
      stop(paste(
        "Confound relations (<->) must be between",
        "nodes contained in the dag"
      ))
    }
    model <- set_confound(model, confound_pairs)
    # overwrite duplication of confound in model statement produced by set_confound
    model$statement <- statement
  }


  # Prep for export
  attr(model, "nonroot_nodes") <- endog_node
  attr(model, "root_nodes")  <- exog_node

  return(model)

}


#' Number of nodal types implied by a parent count: \code{2^(2^k)}
#'
#' Returns \code{Inf} when the value is not representable as a finite
#' double (five or more parents).
#'
#' @param parents integer vector of parent counts
#' @keywords internal
#' @noRd

implied_nodal_type_counts <- function(parents) {
  parents <- as.numeric(parents)
  out <- rep(Inf, length(parents))
  ok <- is.finite(parents) & parents >= 0 & parents <= 4
  out[ok] <- 2^(2^parents[ok])
  # parents == 5 yields 2^32, which is finite in double but too large to
  # materialise; treat parents >= 5 as non-representable here
  out
}


#' Refuse auto-generation of an infeasibly large nodal type set
#'
#' @param n_types named numeric vector of per-node nodal type counts
#' @param max_nodal_types maximum auto-generated types allowed for one node
#' @keywords internal
#' @noRd

check_autogenerated_nodal_types <- function(n_types,
                                            max_nodal_types = 65536) {
  too_big <- !is.finite(n_types) | n_types > max_nodal_types
  if (!any(too_big)) {
    return(invisible(TRUE))
  }
  busiest <- paste(names(n_types)[too_big], collapse = ", ")
  stop("Node(s) ", busiest,
       " imply too many nodal types to auto-generate ",
       "(more than ", format(max_nodal_types, big.mark = ","), ").\n",
       "Supply `nodal_types` explicitly to work with a restricted type space, ",
       "or reduce the number of parents.")
}


#' Guard against combinatorial explosion of causal types
#'
#' The number of causal types is the product of the numbers of nodal types.
#' When that product exceeds \code{max_causal_types} and causal types will be
#' built, error unless \code{allow_large} is \code{TRUE} (warning instead).
#' Non-finite products always error. The causal-type product check is skipped
#' when \code{add_causal_types} is \code{FALSE}, but the parameter-count check
#' (including confound expansion) still runs unless
#' \code{check_parameters = FALSE}.
#'
#' @param n_types numeric vector of per-node nodal type counts
#' @param allow_large logical. Permit products above the soft limit
#' @param add_causal_types logical. Whether causal types will be attached
#' @param max_causal_types soft upper bound on the product (default 1e6)
#' @param confound_pairs optional named list as for \code{set_confound}
#'   (names = expanded node, values = conditioner node)
#' @param max_parameters soft upper bound on parameters after confound
#'   expansion (default 1e6)
#' @param check_parameters logical. Check expanded parameter count
#' @keywords internal
#' @noRd

check_causal_type_count <- function(n_types,
                                    allow_large = FALSE,
                                    add_causal_types = TRUE,
                                    max_causal_types = 1e6,
                                    confound_pairs = NULL,
                                    max_parameters = 1e6,
                                    check_parameters = TRUE) {
  type_names <- names(n_types)
  n_types <- as.numeric(n_types)
  names(n_types) <- type_names
  if (length(n_types) == 0L || anyNA(n_types) || any(n_types < 1)) {
    stop("Invalid nodal type counts.")
  }

  if (any(!is.finite(n_types))) {
    stop("Implied type space is too large to represent.\n",
         "Supply a restricted `nodal_types` list, set `add_causal_types = FALSE`, ",
         "or reduce the model.")
  }

  if (isTRUE(check_parameters)) {
    n_param <- estimate_parameters_with_confound(n_types, confound_pairs)
    if (!is.finite(n_param) || n_param > max_parameters) {
      n_txt <- if (is.finite(n_param)) {
        format(round(n_param), big.mark = ",", scientific = FALSE)
      } else {
        "an astronomical number of"
      }
      msg <- paste0(
        "This model implies ", n_txt, " parameters ",
        "(nodal types",
        if (length(confound_pairs)) " after confound stratification" else "",
        ").\n"
      )
      if (!is.finite(n_param) || !allow_large) {
        stop(msg,
             "Confounding multiplies parameters for stratified nodes. ",
             "Tighten type restrictions (`drop_interactions`, `monotone`, ",
             "`nodal_types`), reduce confounds, or set `allow_large = TRUE`.",
             call. = FALSE)
      }
      warning(msg,
              "Model construction and updating will be slow and memory-hungry.",
              call. = FALSE)
    }
  }

  if (!isTRUE(add_causal_types)) {
    return(invisible(TRUE))
  }

  # log-space product avoids overflow before the comparison
  log_n <- sum(log(n_types))
  n_causal <- if (log_n > log(.Machine$double.xmax)) Inf else exp(log_n)

  if (is.finite(log_n) && log_n <= log(max_causal_types)) {
    return(invisible(TRUE))
  }

  counts_txt <- paste(format(n_types, big.mark = ",", scientific = FALSE),
                      collapse = " x ")
  n_txt <- if (is.finite(n_causal)) {
    format(round(n_causal), big.mark = ",", scientific = FALSE)
  } else {
    "an astronomical number of"
  }

  msg <- paste0(
    "This model implies ", n_txt, " causal types ",
    "(product of nodal type counts: ", counts_txt, ").\n"
  )

  if (!is.finite(n_causal)) {
    stop(msg,
         "Supply a restricted `nodal_types` list, set `add_causal_types = FALSE`, ",
         "or reduce the model.")
  }

  if (!allow_large) {
    stop(msg,
         "Building it takes substantial memory, and `update_model()` will ",
         "typically fail to allocate.\n",
         "Set `allow_large = TRUE` to build it anyway, supply restricted ",
         "`nodal_types`, or set `add_causal_types = FALSE`.")
  }

  warning(msg,
          "Model construction and updating will be slow and memory-hungry.")
  invisible(TRUE)
}


#' Parameter count after set_confound-style stratification.
#'
#' Mirrors \code{set_confound}: for each pair, the expanded node's parameter
#' rows are multiplied by the conditioner node's nodal-type count.
#'
#' @param n_types named numeric vector of nodal type counts
#' @param confound_pairs named list (names = expanded node, values = conditioner)
#' @keywords internal
#' @noRd
estimate_parameters_with_confound <- function(n_types, confound_pairs = NULL) {
  nm <- names(n_types)
  n_types <- as.numeric(n_types)
  names(n_types) <- nm
  n_param <- n_types
  if (!length(confound_pairs)) {
    return(sum(n_param))
  }
  for (i in seq_along(confound_pairs)) {
    expanded <- names(confound_pairs)[[i]]
    given <- as.character(confound_pairs[[i]])
    if (!nzchar(expanded) || !expanded %in% names(n_param)) {
      next
    }
    if (!given %in% names(n_types)) {
      next
    }
    n_param[[expanded]] <- n_param[[expanded]] * n_types[[given]]
  }
  sum(n_param)
}


#' Confound pairs in set_confound order from a dag edge table and node order.
#' @keywords internal
#' @noRd
confound_pairs_from_dag <- function(dag_edges, nodes) {
  z <- dag_edges |>
    dplyr::filter(e == "<->") |>
    dplyr::select(v, w)
  if (!nrow(z)) {
    return(NULL)
  }
  z$v <- as.character(z$v)
  z$w <- as.character(z$w)
  for (i in seq_len(nrow(z))) {
    z[i, ] <- rev(nodes[nodes %in% as.character(z[i, ])])
  }
  confounds <- as.list(as.character(z$w))
  names(confounds) <- z$v
  confounds
}


#' function to make a parameters_df from nodal types
#' @param nodal_types a list of nodal types
#' @keywords internal
#' @examples
#'
#' CausalQueries:::make_parameters_df(list(X = "1", Y = c("01", "10")))

make_parameters_df <- function(nodal_types) {
  pdf <- data.frame(node = rep(names(nodal_types), lapply(nodal_types, length)),
                    nodal_type = nodal_types |> unlist()) |>
    dplyr::mutate(
      param_set = node,
      given = "",
      priors = 1,
      param_names = paste0(node, ".", nodal_type)
    ) |>
    dplyr::group_by(param_set) |>
    dplyr::mutate(param_value = 1 / n(), gen =  cur_group_id()) |>
    dplyr::ungroup() |>
    dplyr::mutate(gen = match(node, names(nodal_types))) |>
    dplyr::select(param_names,
                  node,
                  gen,
                  param_set,
                  nodal_type,
                  given,
                  param_value,
                  priors)

  class(pdf) <- "data.frame"
  return(pdf)
}


#' Helper to clean and check the validity of causal statements specifying a DAG.
#' This function isolates nodes and edges specified in a causal statements and
#' makes them processable by \code{make_dag}
#'
#' @param statement character string. Statement describing causal
#'    relations between nodes.
#' @return a list of nodes and edges specified in the input statement
#' @keywords internal

clean_statement <- function(statement) {
  ## consolidate edges
  statement <- gsub("\\s+", "", statement)
  statement <- strsplit(statement, "")[[1]]

  i <- 1
  st_edge <- c()

  while (i <= length(statement)) {
    # Check for pattern "<", "-", ">"
    if (i <= length(statement) - 2 &&
        statement[i] == "<" &&
        statement[i + 1] == "-" && statement[i + 2] == ">") {
      st_edge <- c(st_edge, "<->")
      i <- i + 3 # Skip the next two elements
    }
    # Check for pattern "<", "-"
    else if (i <= length(statement) - 1 &&
             statement[i] == "<" && statement[i + 1] == "-") {
      st_edge <- c(st_edge, "<-")
      i <- i + 2 # Skip the next element
    }
    # Check for pattern "-", ">"
    else if (i <= length(statement) - 1 &&
             statement[i] == "-" && statement[i + 1] == ">") {
      st_edge <- c(st_edge, "->")
      i <- i + 2 # Skip the next element
    }
    # Otherwise, just append the current element
    else {
      st_edge <- c(st_edge, statement[i])
      i <- i + 1
    }
  }

  # detect edges
  is_edge <- st_edge %in% c("->", "<-", "<->")

  # check for bare edges (i.e. edges without origin or destination nodes)
  # either dangling edge at beginning / end of statement
  has_dangling_edge <- any(c(is_edge[1], is_edge[length(is_edge)]))
  # or consecutive edges within statement
  consecutive_edge <- rle(is_edge)
  has_consecutive_edge <- any(consecutive_edge$values &
                                consecutive_edge$lengths >= 2)

  if (has_dangling_edge || has_consecutive_edge) {
    stop(
      "Statement contains bare edges without a source or destination node or both. Edges should connect nodes."
    )
  }

  ## consolidate nodes
  # check for unsupported characters in varnames
  if (any(c("<", ">") %in% st_edge[!is_edge])) {
    stop(
      paste(
        "Unsupported characters in variable names. No '<' or '>' in variable names please. \n",
        "You may have tried to define an edge but misspecified it.",
        "Edges should be specified via ->, <-, <-> not >, <, <> or ->>, <<-, <<->> etc.",
        sep = " "
      )
    )
  }

  if ("-" %in% st_edge[!is_edge]) {
    stop(
      paste(
        "Unsupported characters in variable names. No hyphens '-' in variable names please; try dots? \n",
        "You may have tried to define an edge but misspecified it.",
        "Edges should be specified via ->, <-, <-> not -.",
        sep = " "
      )
    )
  }

  if ("_" %in% st_edge[!is_edge]) {
    stop(
      paste(
        "Unsupported characters in variable names. No underscores '_' in variable names please; try dots? \n",
        "You may have tried to define an edge but misspecified it.",
        "Edges should be specified via ->, <-, <-> not _>, <_, <_> etc.",
        sep = " "
      )
    )
  }

  # counter that increments every time we hit an edge --> each character
  # belonging to a node thus has the same number (node_id)
  node_id <- cumsum(is_edge)
  # split by node_id and paste all characters with same node_id together
  nodes <- split(st_edge[!is_edge], node_id[!is_edge]) |>
    vapply(paste, collapse = "", "") |>
    unname()

  # ensure that no non-linear mathematical operators are embedded in variable names
  # we have to check this to ensure correct query parsing
  non_linear_operators <- c("\\^", "/", "exp\\(", "log\\(")
  non_linear_warn <- any(
    vapply(non_linear_operators, function(operator) {grepl(operator, nodes)}, logical(length(nodes)))
  )

  if(non_linear_warn) {
    stop(
      paste(
        "Unsupported substrings in variable names. No non-linear mathematical operators like:",
        "^, /, exp( or log( in variable names please. Adding such operator substrings to variable names",
        "will cause downstream issues in query specification and parsing.",
        sep = " "
      )
    )
  }

  # ensure that no query specific syntax is embedded in variable names
  # we have to check this to ensure correct query parsing
  query_operators <- c("\\[", "\\]", ":\\|:")
  query_warn <- any(
    vapply(query_operators, function(operator) {grepl(operator, nodes)}, logical(length(nodes)))
  )

  if(query_warn) {
    stop(
      paste(
        "Unsupported substrings in variable names. No query operators like:",
        "[, ] or :|: in variable names please. Adding such operator substrings to variable names",
        "will cause downstream issues in query specification and parsing.",
        sep = " "
      )
    )
  }

  # Syntactic R names only (avoids ambiguous tokens in confound / type labels)
  bad_names <- nodes[make.names(nodes) != nodes | nodes == ""]
  if (length(bad_names)) {
    stop(
      paste0(
        "Unsupported variable names. Node names must be syntactic R names ",
        "(make.names(x) == x). Problem: ",
        paste(unique(bad_names), collapse = ", ")
      ),
      call. = FALSE
    )
  }

  return(list(nodes = nodes, edges = st_edge[is_edge]))
}


#' Helper to run a causal statement specifying a DAG into a \code{data.frame} of
#' pairwise parent child relations between nodes specified by a respective edge.
#'
#' @param statement character string. Statement describing causal
#'   relations between nodes. Only directed relations are
#'   permitted. For instance "X -> Y" or  "X1 -> Y <- X2; X1 -> X2"
#' @return a \code{data.frame} with columns v, w, e specifying parent, child and
#'   edge respectively
#' @keywords internal

make_dag <- function(statement) {
  # split by ; first to separate discrete parts of the DAG statement
  sub_statements <- strsplit(statement, ";")[[1]]

  dags <- lapply(sub_statements, function(sub_statement) {
    if (sub_statement == "") {
      return(NULL)
    }

    sub_statement <- clean_statement(sub_statement)

    nodes <- sub_statement$nodes
    edges <- sub_statement$edges

    if (length(nodes) == 1) {
      return(NULL)
    }

    dag <- as.data.frame(matrix(NA, length(nodes) - 1, 3))
    colnames(dag) <- c("v", "w", "e")

    for (i in 1:(length(nodes) - 1)) {
      if ((!is.na(edges[i])) && (edges[i] == "<-")) {
        dag[i, "v"] <- nodes[i + 1]
        dag[i, "w"] <- nodes[i]
        dag[i, "e"] <- "->"
      } else {
        dag[i, "v"] <- nodes[i]
        dag[i, "w"] <- nodes[i + 1]
        dag[i, "e"] <- edges[i]
      }
    }

    return(dag)
  })

  dag <- dags[!vapply(dags, is.null, logical(1))]

  if (length(dag) == 0) {
    # Single node case
    data.frame(v = statement, w = NA, e = NA)

  } else {
    dag |>
      dplyr::bind_rows() |>
      dplyr::arrange(v) |>
      distinct() |>
      remove_duplicates()
  }

}



remove_duplicates <- function(df) {
  if (nrow(df) == 1)
    return(df)

  df <- df |> mutate(
    normalized_v = ifelse(e == "<->", pmin(v, w), v),
    normalized_w = ifelse(e == "<->", pmax(v, w), w)
  )
  # Remove duplicates (eg X<-Y; Y<->X)

  df <- df[!duplicated(df[, c("normalized_v", "normalized_w", "e")]), ]

  df[, c("v", "w", "e")]

}
