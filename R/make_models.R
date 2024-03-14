#' Make a model
#'
#' \code{make_model} uses \link{dagitty} syntax and functionality to
#' specify nodes and edges of a graph. Implied causal types are calculated
#' and default priors are provided under the assumption of no confounding.
#' Models can be updated with specification of a parameter matrix, \code{P}, by
#' providing restrictions on causal types, and/or by providing informative
#' priors on parameters. The default setting for a causal model have flat
#' (uniform) priors and parameters putting equal weight on each parameter
#' within each parameter set. These can be adjust with \code{set_priors}
#' and \code{set_parameters}
#'
#' @param statement A character. Statement describing causal
#'   relations using \link{dagitty} syntax. Only directed relations are
#'   permitted. For instance "X -> Y" or  "X1 -> Y <- X2; X1 -> X2".
#' @param add_causal_types Logical. Whether to create and attach causal
#'   types to \code{model}. Defaults to `TRUE`.
#' @param nodal_types List of nodal types associated with model nodes
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
#'   nodal types that produce them}
#' @examples
#' make_model(statement = "X -> Y")
#' modelXKY <- make_model("X -> K -> Y; X -> Y")
#'
#' # Example where cyclicaly dag attempted
#' \dontrun{
#'  modelXKX <- make_model("X -> K -> X")
#' }
#'
#' # Examples with confounding
#' model <- make_model("X->Y; X <-> Y")
#' model$P
#' model <- make_model("Y2 <- X -> Y1; X <-> Y1; X <-> Y2")
#' dim(model$P)
#' model$P
#' model <- make_model("X1 -> Y <- X2; X1 <-> Y; X2 <-> Y")
#' dim(model$P)
#' model$parameters_df
#'
#' # A single node graph is also possible
#' model <- make_model("X")
#'
#' # Unconnected nodes not allowed
#' \dontrun{
#'  model <- make_model("X <-> Y")
#' }
#'
#' nodal_types <-
#'   list(
#'     A = c("0","1"),
#'     B = c("0","1"),
#'     C = c("0","1"),
#'     D = c("0","1"),
#'     E = c("0","1"),
#'     Y = c(
#'       "00000000000000000000000000000000",
#'       "01010101010101010101010101010101",
#'       "00110011001100110011001100110011",
#'       "00001111000011110000111100001111",
#'       "00000000111111110000000011111111",
#'       "00000000000000001111111111111111",
#'       "11111111111111111111111111111111" ))
#'
#' make_model("A -> Y; B ->Y; C->Y; D->Y; E->Y",
#'           nodal_types = nodal_types)$parameters_df
#'
#' nodal_types = list(Y = c("01", "10"), Z = c("0", "1"))
#' make_model("Z -> Y", nodal_types = nodal_types)$parameters_df
#' make_model("Z -> Y", nodal_types = FALSE)$parents_df

make_model <- function(statement,
                       add_causal_types = TRUE,
                       nodal_types = NULL) {

  if (length(statement) != 1) {
    stop(
      paste(
        "The length of the character vector of the statement",
        "is unequal to 1. Please provide only 1 causal model."
      )
    )
  }

  if (!(is.character(statement))) {
    stop("The model statement should be of type character.")
  }

  # names and allowable names
  node_names <- strsplit(statement,
                         paste0(c("->", "<->", "<-", ";"), collapse = "|"),
                         perl = TRUE) |>
    unlist() |>
    trimws() |>
    unique()

  if (any(grepl("-", 	node_names))) {
    stop("No hyphens in varnames please; try dots?")
  }

  if (any(grepl("_", node_names))) {
    stop("No underscores in varnames please; try dots?")
  }

  # generate DAG
  x <-
    dagitty::edges(dagitty::dagitty(paste0("dag{", statement, "}"))) %>%
    data.frame(stringsAsFactors = FALSE)

  if (nrow(x) == 0) {
    dag <- data.frame(v = statement, w = NA)
  } else {
    dag  <- x %>%
      dplyr::filter(e == "->") %>%
      dplyr::select(v, w)
  }

  if (length(x) > 0 &&
      any(!(unlist(x[, 1:2]) %in% unlist(dag)))) {
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
  dag <- dag[order(gen, dag[, 1], dag[, 2]),]

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
    }, numeric(1)))

  # Model is a list
  model <-
    list(
      statement = statement,
      dag = dag,
      nodes = nodes,
      parents_df = parents_df
    )

  # Nodal types
  # Check nodal types map to nodes in model
  if ((!is.null(nodal_types)) && (!all(names(nodal_types) %in% nodes))) {
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

  if (is.null(nodal_types)) {
    nodal_types <- get_nodal_types(model, collapse = TRUE)
  }

  # Add nodal types to model
  model$nodal_types <- nodal_types

  # Add nodal type interpretation
  if (is.null(attr(nodal_types, "interpret"))) {
    attr(model$nodal_types, "interpret") <- interpret_type(model)
  }

  # Parameters dataframe
  if (!is.logical(nodal_types)) {
    model$parameters_df <- make_parameters_df(nodal_types)
  }

  # Add class
  class(model) <- "causal_model"

  # Add causal types
  if (add_causal_types) {
    model$causal_types <- update_causal_types(model)
  }

  # Add confounds if any provided

  if (any(x$e == "<->")) {
    confounds <- NULL

    z  <- x |>
      dplyr::filter(e == "<->") |>
      dplyr::select(v, w)
    z$v <- as.character(z$v)
    z$w <- as.character(z$w)

    # Reorder by reverse causal order (thus in X -> Y we have
    # type_Y conditional on type_X)
    for (i in seq_len(nrow(z))) {
      z[i,] <- rev(nodes[nodes %in% as.character(z[i, ])])
    }
    # Generate confounds list
    confounds <- as.list(as.character(z$w))
    names(confounds) <- z$v

    # Check on ineligible confound statements
    if (any(!(c(z$v, z$w) %in% nodes))) {
      stop(paste(
        "Confound relations (<->) must be between",
        "nodes contained in the dag"
      ))
    }
    model <- set_confound(model, confounds)
  }

  # Prep for export
  attr(model, "nonroot_nodes") <- endog_node
  attr(model, "root_nodes")  <- exog_node

  return(model)

}

#' @export
print.causal_model <- function(x, ...) {
  cat("\nStatement: \n")
  print(x$statement)

  cat("\nDAG: \n")
  print(x$dag)
	invisible(x)

	cat("\nNumber of types by node:\n")
	nodal_types <- get_nodal_types(x)
	print(vapply(nodal_types , length, numeric(1) ,USE.NAMES = TRUE))


	if (!is.null(x$causal_types)) {
	  cat("\nNumber of causal types:")
	  cat(paste0("  ", nrow(get_causal_types(x)), "\n"))
	}

	invisible(x)
}


#' @export
summary.causal_model <- function(object, ...) {
	structure(object, class = c("summary.causal_model", "data.frame"))

}

#' @export
print.summary.causal_model <- function(x,  ...) {
  cat("\nStatement: \n")
  print(x$statement)
  cat("\nDAG: \n")
  print(x$dag)
  cat("\n ------------------------------------------------------------------\n")
  cat("\nNodal types: \n")

  nodal_types <- get_nodal_types(x)
  nodes <- x$nodes

  for (n in nodes) {
    nt <- nodal_types[[n]]
    interpret <- attr(nodal_types, "interpret")[[n]]
    stop_at <- min(length(nt), 16)
    cat(paste0("$", n, "\n"))

    cat(paste0(nt[1:stop_at], collapse = "  "))

    if (stop_at != length(nt)) {
      cat(paste0(" ...", length(nt) - 16, " nodal types omitted"))
    }
    cat("\n\n")
    print(interpret)
    cat("\n")
  }

  cat("\nNumber of types by node\n")

  print(vapply(nodal_types , length, numeric(1) ,USE.NAMES = TRUE))


  if (!is.null(x$causal_types)) {
    cat("\nNumber of causal types:")
    cat(paste0("  ", nrow(get_causal_types(x)), "\n"))
  }

  if (!is.null(attr(x, "restrictions"))) {
    restrictions <- attr(x, "restrictions")
    cat("\n ----------------------------------------------------------------\n")
    cat("\nRestrictions: \n")
    for (node in x$nodes) {
      cat(paste0(node,
                 ": ",
                 length(restrictions[[node]]),
                 " restricted types \n"))
    }
  }
}


#' function to make a parameters_df from nodal types
#' @param nodal_types a list of nodal types
#' @export
#' @keywords internal
#' @examples
#'
#' make_parameters_df(list(X = "1", Y = c("01", "10")))

make_parameters_df <- function(nodal_types){
  pdf <- data.frame(node = rep(names(nodal_types), lapply(nodal_types, length)),
                    nodal_type = nodal_types %>% unlist) |>
    dplyr::mutate(param_set = node,
           given = "",
           priors = 1,
           param_names = paste0(node, ".", nodal_type)) |>
    dplyr::group_by(param_set) |>
    dplyr::mutate(param_value = 1/n(), gen =  cur_group_id()) |>
    dplyr::ungroup() |>
    dplyr::mutate(gen = match(node, names(nodal_types))) |>
    dplyr::select(param_names, node, gen, param_set, nodal_type,
                  given, param_value, priors)

  return(pdf)
}



