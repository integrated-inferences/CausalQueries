#' Make a model
#'
#' \code{make_model} uses causal statements encoded as strings to specify
#' the nodes and edges of a graph. Implied causal types are calculated
#' and default priors are provided under the assumption of no confounding.
#' Models can be updated with specification of a parameter matrix, \code{P}, by
#' providing restrictions on causal types, and/or by providing informative
#' priors on parameters. The default setting for a causal model have flat
#' (uniform) priors and parameters putting equal weight on each parameter
#' within each parameter set. These can be adjust with \code{set_priors}
#' and \code{set_parameters}
#'
#' @param statement character string. Statement describing causal
#'   relations between nodes. Only directed relations are
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
#'
#'
#' @seealso \code{\link{summary.causal_model}} provides summary method for
#'   output objects of class \code{causal_model}
#'
#'
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
#'           nodal_types = nodal_types) |>
#'  inspect("parameters_df")
#'
#' nodal_types = list(Y = c("01", "10"), Z = c("0", "1"))
#' make_model("Z -> Y", nodal_types = nodal_types) |>
#'  inspect("parameters_df")


make_model <- function(statement,
                       add_causal_types = TRUE,
                       nodal_types = NULL) {

  parent <- NULL

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
    }, numeric(1))) |>
    dplyr::mutate(parent_nodes = sapply(node, function(n) {
      dag |>
        dplyr::filter(children == n) |>
        dplyr::pull(parent) |>
        paste(collapse = ", ")
    }))

  # Model is a list
  model <-
    list(
      statement = statement,
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

  # Parameters data frame
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

  if (grepl("<->", statement)) {
    confounds <- NULL

    z  <- .dag |>
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
    # overwrite duplication of confound in model statement produced by set_confound
    model$statement <- statement
  }


  # Prep for export
  attr(model, "nonroot_nodes") <- endog_node
  attr(model, "root_nodes")  <- exog_node


  # assign classes
  class(model$statement) <- "character"
  class(model$nodes) <- "character"
  class(model$parents_df) <- "data.frame"
  class(model$nodal_types) <- "list"

  return(model)

}


#' function to make a parameters_df from nodal types
#' @param nodal_types a list of nodal types
#' @keywords internal
#' @examples
#'
#' CausalQueries:::make_parameters_df(list(X = "1", Y = c("01", "10")))

make_parameters_df <- function(nodal_types){
  pdf <- data.frame(node = rep(names(nodal_types), lapply(nodal_types, length)),
                    nodal_type = nodal_types |> unlist()) |>
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
  has_consecutive_edge <- any(consecutive_edge$values & consecutive_edge$lengths >= 2)

  if(has_dangling_edge || has_consecutive_edge) {
    stop("Statement contains bare edges without a source or destination node or both. Edges should connect nodes.")
  }

  ## consolidate nodes
  # check for unsupported characters in varnames
  if(any(c("<",">") %in% st_edge[!is_edge])) {
    stop(
      paste0(
        "Unsupported characters in variable names. No '<' or '>' in variable names please.",
        "\n",
        "\n You may have tried to define an edge but misspecified it.",
        "\n Edges should be specified via ->, <-, <-> not >, <, <> or ->>, <<-, <<->> etc."
      )
    )
  }

  if("-" %in% st_edge[!is_edge]) {
    stop(
      paste0(
        "Unsupported characters in variable names. No hyphens '-' in variable names please; try dots?",
        "\n",
        "\n You may have tried to define an edge but misspecified it.",
        "\n Edges should be specified via ->, <-, <-> not -."
      )
    )
  }

  if("_" %in% st_edge[!is_edge]) {
    stop(
      paste0(
        "Unsupported characters in variable names. No underscores '_' in variable names please; try dots?",
        "\n",
        "\n You may have tried to define an edge but misspecified it.",
        "\n Edges should be specified via ->, <-, <-> not _>, <_, <_> etc."
      )
    )
  }

  if(("<->" %in% st_edge[is_edge]) && ("." %in% st_edge[!is_edge])) {
    stop(
      "Unsupported characters in variable names. No dots '.' in variable names for models with confounding."
    )
  }

  # counter that increments every time we hit an edge --> each character
  # belonging to a node thus has the same number (node_id)
  node_id <- cumsum(is_edge)
  # split by node_id and paste all characters with same node_id together
  nodes <- split(st_edge[!is_edge], node_id[!is_edge]) |>
    vapply(paste, collapse = "", "") |>
    unname()

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

    if(sub_statement == "") {
      return(NULL)
    }

    sub_statement <- clean_statement(sub_statement)

    nodes <- sub_statement$nodes
    edges <- sub_statement$edges

    if(length(nodes) == 1) {
      return(NULL)
    }

    dag <- as.data.frame(matrix(NA, length(nodes) - 1, 3))
    colnames(dag) <- c("v","w","e")

    for(i in 1:(length(nodes) - 1)) {
      if((!is.na(edges[i])) && (edges[i] == "<-")) {
        dag[i,"v"] <- nodes[i+1]
        dag[i,"w"] <- nodes[i]
        dag[i,"e"] <- "->"
      } else {
        dag[i,"v"] <- nodes[i]
        dag[i,"w"] <- nodes[i+1]
        dag[i,"e"] <- edges[i]
      }
    }

    return(dag)
  })

  dag <- dags[!vapply(dags, is.null, logical(1))]

  if(length(dag) == 0) {
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
  if(nrow(df) == 1) return(df)

  df <- df |> mutate(
    normalized_v = ifelse(e == "<->", pmin(v, w), v),
    normalized_w = ifelse(e == "<->", pmax(v, w), w)
  )
  # Remove duplicates (eg X<-Y; Y<->X)

  df <- df[!duplicated(df[, c("normalized_v", "normalized_w", "e")]), ]

  df[, c("v", "w", "e")]

}



