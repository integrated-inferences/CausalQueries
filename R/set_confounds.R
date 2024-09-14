#' Set confound
#'
#' Adjust parameter matrix to allow confounding.
#'
#'
#' Confounding between X and Y arises when the nodal types for X and Y are not
#' independently distributed. In the X -> Y graph, for instance, there are 2
#' nodal types for X and 4 for Y. There are thus 8 joint nodal types:
#' \preformatted{
#' |          | t^X                |                    |           |
#' |-----|----|--------------------|--------------------|-----------|
#' |     |    | 0                  | 1                  | Sum       |
#' |-----|----|--------------------|--------------------|-----------|
#' | t^Y | 00 | Pr(t^X=0 & t^Y=00) | Pr(t^X=1 & t^Y=00) | Pr(t^Y=00)|
#' |     | 10 | .                  | .                  | .         |
#' |     | 01 | .                  | .                  | .         |
#' |     | 11 | .                  | .                  | .         |
#' |-----|----|--------------------|--------------------|-----------|
#' |     |Sum | Pr(t^X=0)          | Pr(t^X=1)          | 1         |
#' }
#'
#' This table has 8 interior elements and so an unconstrained joint
#' distribution would have 7 degrees of freedom. A no confounding assumption
#' means that Pr(t^X | t^Y) = Pr(t^X), or  Pr(t^X, t^Y) = Pr(t^X)Pr(t^Y).
#' In this case there would be 3 degrees of freedom for Y and 1 for X,
#' totaling 4 rather than 7.
#'
#' \code{set_confound} lets you relax this assumption by increasing the
#' number of parameters characterizing the joint distribution. Using the fact
#' that P(A,B) = P(A)P(B|A) new parameters are introduced to capture P(B|A=a)
#' rather than simply P(B). For instance here two parameters
#' (and one degree of freedom) govern the distribution of types X  and four
#' parameters (with 3 degrees of freedom) govern  the types for Y given the
#' type of X for a total of 1+3+3 = 7 degrees of freedom.
#'
#'
#' @inheritParams CausalQueries_internal_inherit_params
#' @param confound A \code{list} of statements indicating pairs of nodes whose
#'   types are jointly distributed (e.g. list("A <-> B", "C <-> D")).
#' @return An object of class \code{causal_model} with updated parameters_df
#'   and parameter matrix.
#' @export
#' @examples
#'
#' make_model('X -> Y; X <-> Y') |>
#' grab("parameters")
#'
#' make_model('X -> M -> Y; X <-> Y') |>
#' grab("parameters")
#'
#' model <- make_model('X -> M -> Y; X <-> Y; M <-> Y')
#' inspect(model, "parameters_df")
#'
#' # Example where set_confound is implemented after restrictions
#'make_model("A -> B -> C") |>
#' set_restrictions(increasing("A", "B")) |>
#' set_confound("B <-> C") |>
#' grab("parameters")
#'
#' # Example where two parents are confounded
#' make_model('A -> B <- C; A <-> C') |>
#'   set_parameters(node = "C", c(0.05, .95, .95, 0.05)) |>
#'   make_data(n = 50) |>
#'   cor()
#'
#'  # Example with two confounds, added sequentially
#' model <- make_model('A -> B -> C') |>
#'   set_confound(list("A <-> B", "B <-> C"))
#' inspect(model, "causal_statement")
#' # plot(model)


set_confound <- function(model,
                         confound = NULL) {
  # handle global variables
  x <- NULL

  is_a_model(model)

  if (any(confound |> lapply(function(k)
    grepl(";", k)) |> unlist())) {
    stop("Please provide multipe confounds as a list")
  }

  given <- gen <- NULL

  if (!(all(model$parameters_df$given == ""))) {
    stop(paste(
      "Confounds have already been declared.",
      "Please declare confounds only once."
    ))
  }

  # extract the node from the nodal type name
  node_from_type <- function(type) {
    vapply(strsplit(type, "\\."), function(x) {
      x[[1]]
    }, character(1))
  }

  # extract the conditions from the nodal type name
  conditions_from_type <- function(type) {
    lapply(strsplit(type, "_"), function(x)
      gsub('\\.', '', unique(x)))
  }

  # Housekeeping
  if (is.null(confound)) {
    message("No confound provided")
    return(model)
  }

  if (is.null(model$P)) {
    model <- set_parameter_matrix(model)
  }

  # Turn A <-> B format to lists
  for (j in seq_along(confound)) {
    if (grepl("<->", confound[[j]])) {
      z <- vapply(strsplit(confound[[j]], "<->"), trimws, character(2))
      z <- rev(model$nodes[model$nodes %in% as.character(z)])
      confound[j] <- as.character(z[2])
      names(confound)[j] <- z[1]
    }
  }


  # Add confounds to model statement
  model$statement <- paste0(
    model$statement,
    "; ",
    paste(names(confound), "<->", confound, collapse = "; ")
  )


  # Expand parameters_df ------------------------------------------------------
  names_P <- names(model$P)

  for (i in seq_along(confound)) {
    from_nodal_types <-
      model$parameters_df |>
      dplyr::filter(node == confound[i]) |>
      dplyr::mutate(x = paste0(node, ".", nodal_type)) |>
      dplyr::pull(x) |>
      unique()

    to_add <-
      lapply(from_nodal_types, function(j)
        model$parameters_df |>
          dplyr::filter(node == names(confound)[i]) |>
          dplyr::mutate(
            given = ifelse(given == "", j, paste0(given, ", ", j)),
            param_names = paste0(param_names, "_", j),
            param_set = paste0(param_set, ".", j)
          )) |>
      dplyr::bind_rows()

    model$parameters_df <-
      rbind(filter(model$parameters_df, node != names(confound)[i]),
            to_add) |>
      dplyr::arrange(gen, param_set)
  }


  # P matrix expand -----------------------------------------------------------

  for (i in seq_along(confound)) {
    from_nodal_types <-
      model$parameters_df |>
      dplyr::filter(node == confound[i]) |>
      dplyr::mutate(x = paste0(node, ".", nodal_type)) |>
      dplyr::pull(x) |>
      unique()

    to_add <-
      lapply(from_nodal_types, function(j) {
        newP <- model$P |>
          dplyr::filter(node_from_type(rownames(model$P)) == names(confound)[i])
        rownames(newP) <- paste0(rownames(newP), "_", j)

        # delete relevant entries: need to figure if *all* conditioning
        # nodes are in observed data. Sometimes never: eg:"X.1_Y.00_X.0"
        # ie. (Z| (X1), (Y00|X0)
        row_elements <- conditions_from_type(rownames(newP))

        for (k in seq_along(row_elements)) {
          to_zero <- vapply(row_elements[k][[1]], function(nd) {
            vapply(nd, function(ndd) {
              grepl(ndd, names_P)
            }, logical(length(names_P)))
          }, logical(length(names_P))) |>
            apply(1, prod)
          newP[k, to_zero == 0] <- 0
        }

        newP
      }) |>
      dplyr::bind_rows()

    # Remove impossible rows with all zeros
    to_add <- filter(to_add, apply(to_add, 1, sum) != 0)

    # Add in
    model$P <- model$P |>
      dplyr::filter(node_from_type(rownames(model$P)) != names(confound)[i]) |>
      rbind(to_add)
  }

  # Clean up ------------------------------------------------------------------
  # Remove existing distributions
  # Distributions are no longer valid
  if (!is.null(model$prior_distribution)) {
    model$prior_distribution <- NULL
  }

  if (!is.null(model$posterior_distribution)) {
    model$posterior_distribution <- NULL
  }

  if (!is.null(model$stan_objects)) {
    model$stan_objects <- NULL
  }

  # P reorder
  model$parameters_df <- model$parameters_df |>
    dplyr::filter(param_names %in% row.names(model$P))

  model$P <-
    model$P[match(model$parameters_df$param_names, rownames(model$P)), ]

  class(model$P) <- c("parameter_matrix", "data.frame")
  rownames(model$parameters_df) <- NULL

  # Export
  attr(model$P, "param_set") <- unique(model$parameters_df$param_set)
  return(model)
}

set_confounds <- set_confound


