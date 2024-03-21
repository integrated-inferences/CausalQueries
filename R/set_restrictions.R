#' Restrict a model
#'
#' Restrict a model's parameter space. This reduces the number of nodal types
#' and in consequence the number of unit causal types.
#'
#' Restrictions are made to nodal types, not to unit causal types.
#' Thus for instance in a model \code{X -> M -> Y}, one cannot apply a simple
#' restriction so that \code{Y} is nondecreasing in  \code{X}, however one can
#' restrict so that \code{M} is nondecreasing in \code{X} and \code{Y}
#' nondecreasing in \code{M}. To have a restriction that \code{Y} be
#' nondecreasing in \code{X} would otherwise require restrictions on
#' causal types, not nodal types, which implies a form of undeclared
#' confounding (i.e. that in cases in which \code{M} is decreasing in \code{X},
#' \code{Y} is decreasing in \code{M}).
#'
#' Since restrictions are to nodal types, all parents of a node are
#' implicitly fixed.  Thus for model \code{make_model(`X -> Y <- W`)} the
#' request \code{set_restrictions(`(Y[X=1] == 0)`)} is interpreted as
#' \code{set_restrictions(`(Y[X=1, W=0] == 0 | Y[X=1, W=1] == 0)`)}.
#'
#' Statements with implicitly controlled nodes should be surrounded by
#' parentheses, as in these examples.
#'
#' Note that prior probabilities are redistributed over remaining types.
#'
#' @inheritParams CausalQueries_internal_inherit_params
#' @param statement  A quoted expressions defining the restriction.
#'   If values for some parents are not specified, statements should be
#'   surrounded by parentheses, for instance \code{(Y[A = 1] > Y[A=0])} will
#'   be interpreted for all combinations of other parents of Y set at possible
#'   levels they might take.
#' @param join_by A string. The logical operator joining expanded types when
#'   \code{statement} contains wildcard (\code{.}). Can take values
#'   \code{'&'} (logical AND) or \code{'|'} (logical OR). When restriction
#'   contains wildcard (\code{.}) and \code{join_by} is not specified, it
#'   defaults to \code{'|'}, otherwise it defaults to \code{NULL}. Note that
#'   join_by joins within statements, not across statements.
#' @param labels A list of character vectors specifying nodal types to be kept
#'   or removed from the model. Use \code{get_nodal_types} to see syntax.
#'   Note that \code{labels} gets overwritten by \code{statement} if
#'   \code{statement} is not NULL.
#' @param param_names A character vector of names of parameters to restrict on.
#' @param given A character vector or list of character vectors specifying
#'   nodes on which the parameter set to be restricted depends.
#'   When restricting by \code{statement}, \code{given} must either be
#'   \code{NULL} or of the same length as \code{statement}. When mixing
#'   statements that are further restricted by \code{given} and ones that are
#'   not, statements without \code{given} restrictions should have \code{given}
#'   specified as one of \code{NULL}, \code{NA}, \code{""} or \code{" "}.
#' @param keep Logical. If `FALSE`, removes and if `TRUE` keeps only causal
#'   types specified by \code{statement} or \code{labels}.
#'
#' @family restrictions
#'
#' @export
#' @return An object of class \code{model}. The causal types and nodal types
#'   in the model are reduced according to the stated restriction.
#'
#' @examples
#'
#' # 1. Restrict parameter space using statements
#' model <- make_model('X->Y') %>%
#'   set_restrictions(statement = c('X[] == 0'))
#'
#' model <- make_model('X->Y') %>%
#'   set_restrictions(non_increasing('X', 'Y'))
#'
#' model <- make_model('X -> Y <- W') %>%
#'   set_restrictions(c(decreasing('X', 'Y'), substitutes('X', 'W', 'Y')))
#'
#' model$parameters_df
#'
#' model <- make_model('X-> Y <- W') %>%
#'   set_restrictions(statement = decreasing('X', 'Y'))
#' model$parameters_df
#'
#' model <- make_model('X->Y') %>%
#'   set_restrictions(decreasing('X', 'Y'))
#' model$parameters_df
#'
#' model <- make_model('X->Y') %>%
#'   set_restrictions(c(increasing('X', 'Y'), decreasing('X', 'Y')))
#' model$parameters_df
#' \donttest{
#' # Restrict to define a model with monotonicity
#' model <- make_model('X->Y') %>%
#' set_restrictions(statement = c('Y[X=1] < Y[X=0]'))
#' get_parameter_matrix(model)
#'
#' # Restrict to a single type in endogenous node
#' model <- make_model('X->Y') %>%
#' set_restrictions(statement =  '(Y[X = 1] == 1)', join_by = '&', keep = TRUE)
#' get_parameter_matrix(model)
#'
#'#  Use of | and &
#'# Keep node if *for some value of B* Y[A = 1] == 1
#'model <- make_model('A->Y<-B') %>%
#' set_restrictions(statement =  '(Y[A = 1] == 1)', join_by = '|', keep = TRUE)
#'dim(get_parameter_matrix(model))
#'
#'
#'# Keep node if *for all values of B* Y[A = 1] == 1
#'model <- make_model('A->Y<-B') %>%
#' set_restrictions(statement =  '(Y[A = 1] == 1)', join_by = '&', keep = TRUE)
#' dim(get_parameter_matrix(model))
#'
#' # Restrict multiple nodes
#' model <- make_model('X->Y<-M; X -> M' ) %>%
#' set_restrictions(statement =  c('(Y[X = 1] == 1)', '(M[X = 1] == 1)'),
#'                  join_by = '&', keep = TRUE)
#' get_parameter_matrix(model)
#'
#' # Restrict using statements and given:
#' model <- make_model("X -> Y -> Z; X <-> Z") %>%
#'  set_restrictions(list(decreasing('X','Y'), decreasing('Y','Z')),
#'                   given = c(NA,'X.0'))
#' get_parameter_matrix(model)
#'
#' # Restrictions on levels for endogenous nodes aren't allowed
#' \dontrun{
#' model <- make_model('X->Y') %>%
#' set_restrictions(statement =  '(Y == 1)')
#' }
#'
#' # 2. Restrict parameter space Using labels:
#' model <- make_model('X->Y') %>%
#' set_restrictions(labels = list(X = '0', Y = '00'))
#'
#' # Restrictions can be  with wildcards
#' model <- make_model('X->Y') %>%
#' set_restrictions(labels = list(Y = '?0'))
#' get_parameter_matrix(model)
#'
#' # Deterministic model
#' model <- make_model('S -> C -> Y <- R <- X; X -> C -> R') %>%
#' set_restrictions(labels = list(C = '1000', R = '0001', Y = '0001'),
#'                  keep = TRUE)
#' get_parameter_matrix(model)
#'
#' # Restrict using labels and given:
#' model <- make_model("X -> Y -> Z; X <-> Z") %>%
#'  set_restrictions(labels = list(X = '0', Z = '00'), given = c(NA,'X.0'))
#' get_parameter_matrix(model)
#'}

set_restrictions <- function(model,
                             statement = NULL,
                             join_by = "|",
                             labels = NULL,
                             param_names = NULL,
                             given = NULL,
                             keep = FALSE) {

  is_a_model(model)

  # Save entering features
  nodal_types_0 <- model$nodal_types
  restrictions_0 <- attr(model, "restrictions")


  if (!is.logical(keep)) {
    stop("`keep` should be either 'TRUE' or 'FALSE'")
  }

  if (sum(!is.null(labels), !is.null(statement), !is.null(param_names)) != 1) {
    stop(
      paste("Provide *either* a causal statement or",
             "nodal type labels or param_names.")
      )
  }

  #check given
  if (!is.null(given)) {

    given[vapply(given, is.null, logical(1))] <- NA
    given_test <- vapply(given, function(i) {
      (any(!is.na(i)) & !is.character(i))
    }, logical(1))

    if (any(given_test)) {
      stop(
        paste(
          "Error in given argument: ",
          paste(which(given_test), collapse = ","),
          ". Given arguments must either be of type character or NULL or NA.",
          sep = ""
        )
      )
    }

    if ((!is.null(statement)) && (length(statement) != length(given))) {
      stop(
        paste(
          "Mismatch in statement and given. Please specify a given for each",
          "statement. For statements that should not have an attached given,",
          "specify given as any of NA,NULL,'',' '."
        )
      )
    }

    if ((!is.null(labels)) && (length(labels) != length(given))) {
      stop(
        paste(
          "Mismatch in label and given. Please specify a given for each label.",
          "For labels that should not have an attached given, specify given as",
          "any of NA,NULL,'',' '."
        )
      )
    }

    given <- vapply(given, trimws, character(1))
    given[vapply(given, function(i) all(i == ""), logical(1))] <- NA

    given_test <- vapply(given, function(i) {
      !is.na(i) & !(i %in% model$parameters_df$given)
    }, logical(1))

    if (any(given_test)) {
      stop(paste(
        "Error in given: ",
        paste(which(given_test), collapse = ", "),
        ". ",
        paste(unlist(mapply(
          function(l, b)
            l[b], given, given_test
        )), collapse = ", "),
        " : not part of given.",
        sep = ""
      ))
    }

  }

  # 1 param_names
  if (!is.null(param_names)) {
    if (!keep) {
      drop_params <- param_names
    }

    if (keep) {
      drop_params <-
        model$parameters_df$param_names[
          !(model$parameters_df$param_names %in% param_names)]
    }
  }

  # 2 Labels
  if (!is.null(labels)) {
    drop_params <- restrict_by_labels(
      model,
      labels = labels,
      keep = keep,
      given = given
    )
  }

  # 3 Statement
  if (!is.null(statement)) {
    drop_params <- restrict_by_query(
      model,
      statement = statement,
      join_by = join_by,
      given = given,
      keep = keep
    )
  }

  ## Clean up
  if (is.null(model$P)) {
    model <- set_parameter_matrix(model)
  }


  # update P
  # remove parameters implicated by restrictions from P (remove rows from P)
  model$P <-
    model$P[!(rownames(model$P) %in% drop_params),]
  model$parameters_df <-
    model$parameters_df[!(model$parameters_df$param_names %in% drop_params),]

  # remove causal types without component nodal types (remove columns from P)
  keep_ct <-
    model$P |>
    dplyr::group_by(g = model$parameters_df$node) |>
    dplyr::mutate(across(.cols = everything(), ~ max(.x))) |>
    dplyr::slice(1) |>
    dplyr::ungroup() |>
    dplyr::select(-g) |>
    apply(2, prod)

  model$P <- dplyr::select(model$P, names(keep_ct)[keep_ct == 1])

  #remove empty parameter families
  sets <- unique(model$parameters_df$param_set)
  keep_f <- vapply(sets, function(j) {
    sum(model$P[model$parameters_df$param_set == j, ]) > 0
  }, logical(1))

  if (min(keep_f) == 0) {
    model$P <-
      model$P[model$parameters_df$param_set %in% names(keep_f)[keep_f],]
  }


  # update parameters df
  model$parameters_df <-
    model$parameters_df[
      model$parameters_df$param_names %in% rownames(model$P),] |>
    clean_params(warning = FALSE)

  # update causal types
  model$causal_types <- model$causal_types[names(keep_ct)[keep_ct == 1],]

  # update nodal types
  model$nodal_types <-
    lapply(model$nodes, function(i) {
      unique(model$parameters_df[model$parameters_df$node == i,]$nodal_type)
    })

  names(model$nodal_types) <- model$nodes
  attr(model$nodal_types, "interpret") <- interpret_type(model)


  # Keep restricted types as attributes
  restrictions <- lapply(model$nodes, function(node) {
    restricted <-
      !(nodal_types_0[[node]] %in% model$nodal_types[[node]])
    nodal_types_0[[node]][restricted]
  })

  names(restrictions) <- model$nodes

  restrictions <- Filter(length, restrictions)

  attr(model, "restrictions") <- {
    if (is.null(restrictions_0)) {
      restrictions
    } else {
      restriction_list <- lapply(model$nodes, function(node) {
        c(restrictions[[node]], restrictions_0[[node]])
      })
      names(restriction_list) <- model$nodes
      restriction_list
    }
  }

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

  return(model)
}

#' Reduce nodal types using statement
#'
#' @param model a model created by make_model()
#' @param statement a list of character vectors specifying nodal types to be
#'   removed from the model. Use \code{get_nodal_types} to see syntax.
#' @param join_by A string or a list of strings. The logical operator joining
#'   expanded types when \code{statement} contains wildcard (\code{.}).
#'   Can take values \code{'&'} (logical AND) or \code{'|'} (logical OR).
#'   When restriction contains wildcard (\code{.}) and \code{join_by} is not
#'   specified, it defaults to \code{'|'}, otherwise it defaults to \code{NULL}.
#' @param given A character vector or list of character vectors specifying
#'   nodes on which the parameter set to be restricted depends. \code{given}
#'   must either be NULL or of the same length as \code{statement}. When mixing
#'   statements that are further restricted by \code{given} and ones that are
#'   not, statements without \code{given} restrictions should have \code{given}
#'   specified as one of \code{NULL}, \code{NA}, \code{""} or \code{" "}.
#' @param keep Logical. If `FALSE`, removes and if `TRUE` keeps only
#'   causal types specified by \code{restriction}.
#' @return A list with two components: 1. a vector with parameters names of
#'   parameters implicated by the restrictions, 2. a vector of subsetting
#'   instructions used to identify implicated causal types
#' @keywords internal
#' @family restrictions

restrict_by_query <- function(model,
                              statement,
                              join_by = "|",
                              given = NULL,
                              keep = FALSE) {
  nodal_types <- model$nodal_types

  n_restrictions <- length(statement)

  if (length(join_by) == 1) {
    join_by <- rep(join_by, n_restrictions)
  } else if (length(join_by) != n_restrictions) {
    stop(
      paste(
        "Argument `join_by` must be either of length 1 or have the same",
        "lenght as `restriction` argument."
      )
    )
  }

  drop_params <- vector(mode = "list", length = length(statement))

  for (i in seq_along(statement)) {
    restriction <-
      map_query_to_nodal_type(model, query = statement[[i]], join_by[i])
    node <- restriction$node
    types <- names(restriction$types)[restriction$types]
    names(types) <- node

    if (!keep) {
      types <- setdiff(nodal_types[[node]], types)
    }

    drop <-
      (model$parameters_df$node %in% node) &
      !(model$parameters_df$nodal_type %in% types)

    if (!is.null(given)) {
      if (all(!is.na(given[[i]]))) {
        wrong_given <-
          !(given[[i]] %in% unlist(model$parameters_df[drop, "given"]))

        if (any(wrong_given)) {
          stop(
            paste(
              "Error in given set ",
              i,
              ". ",
              "Given ",
              paste(given[[i]][wrong_given], collapse = ", "),
              " not part of the restriction set defined by statement. ",
              "Check model$parameters_df for dependence structure between",
              "parameters."
            )
          )
        }

        drop <- drop & (model$parameters_df$given %in% given[[i]])
      }
    }

    drop_params[[i]] <- model$parameters_df[drop,]$param_names
  }

  unlist(drop_params)
}

#' Reduce nodal types using labels
#'
#' @inheritParams CausalQueries_internal_inherit_params
#'
#' @param labels A list of character vectors specifying nodal types to be
#'   kept or removed from the model.
#' @param given A character vector or list of character vectors specifying
#'   nodes on which the parameter set to be restricted depends. When mixing
#'   labels that are further restricted by \code{given} and ones that are not,
#'   labels without \code{given} restrictions should have \code{given}
#'   specified as one of \code{NULL}, \code{NA}, \code{""} or \code{" "}.
#' @param keep Logical. If `FALSE`, removes and if `TRUE` keeps only causal
#'   types specified by \code{restriction}.
#' @return A list with two components: 1. a vector with parameters names of
#'   parameters implicated by the restrictions, 2. a vector of subsetting
#'   instructions used to identify implicated causal types
#' @keywords internal
#' @family restrictions

restrict_by_labels <- function(model,
                               labels,
                               given = NULL,
                               keep = FALSE) {
  # Stop if none of the names of the labels vector matches nodes in dag
  # Stop if there's any labels name that doesn't match any of the nodes
  # in the dag
  restricted_vars <- names(labels)
  matches <- restricted_vars %in% model$nodes

  if (!any(matches)) {
    stop("Variables ",
         paste(names(labels[!matches]), "are not part of the model."))
  }

  # If there are wild cards, spell them out
  labels <- lapply(labels, function(j) {
    lapply(j, unpack_wildcard) |>
      unlist() |>
      as.vector() |>
      unique()
  })


  # Check if labels map to nodal types
  for (i in restricted_vars) {
    check <- model$parameters_df |>
      dplyr::filter(node == i)

    labels_passed <- unlist(labels[[i]])

    if (!all (labels_passed %in% check$nodal_type)) {
      wrong_labels <-
        paste(labels_passed[!(labels_passed %in% check$nodal_type)],
              collapse = ", ")
      stop(
        paste(
          "Error in label set ",
          i,
          ". ",
          "Nodal types ",
          wrong_labels,
          " do not exist for node ",
          i,
          ". ",
          "Check model$parameters_df for correct mapping of labels ",
          "to nodal_types.",
          sep = ""
        )
      )
    }

  }

  drop_params <-
    vector(mode = "list", length = length(restricted_vars))

  # Reduce nodal_types
  for (k in seq_along(restricted_vars)) {
    j <- restricted_vars[k]
    nt <- model$nodal_types[[j]]

    if (!keep) {
      to_keep <- nt[!(nt %in% labels[[k]])]
    }

    if (keep) {
      to_keep <- nt[nt %in% labels[[k]]]
    }

    drop <-
      model$parameters_df$node == j &
      !(model$parameters_df$nodal_type %in% to_keep)

    if (!is.null(given)) {
      if (all(!is.na(given[[k]]))) {
        wrong_given <-
          !(given[[k]] %in% unlist(model$parameters_df[drop, "given"]))

        if (any(wrong_given)) {
          stop(
            paste(
              "Error in given set ",
              k,
              ". Given",
              paste(given[[k]][wrong_given], collapse = ", "),
              "not part of the restriction set defined by labels.",
              "Check model$parameters_df for dependence structure between",
              "parameters."
            )
          )
        }
        drop <- drop & (model$parameters_df$given %in% given[[k]])
      }
    }

    drop_params[[k]] <- model$parameters_df[drop, ]$param_names
  }

  unlist(drop_params)
}


#' Get type names
#' @param nodal_types Nodal types of a model. See \code{\link{get_nodal_types}}.
#' @return A vector containing causal type names
#' @keywords internal
#' @examples
#' model <- make_model('A->Y<-B')
#'CausalQueries:::get_type_names(model$nodal_types)

get_type_names <- function(nodal_types) {
    unlist(lapply(seq_along(nodal_types), function(i) {
        name <- names(nodal_types)[i]
        types <- nodal_types[[i]]
        paste(name, types, sep = ".")
    }))
}


#' Unpack a wild card
#' @param x A character. A nodal type containing one or more wildcard
#'   characters '?' to be unpacked.
#' @return A type label with wildcard characters '?' substituted by 0 and 1.
#' @keywords internal


unpack_wildcard <- function(x) {
    splitstring <- strsplit(x, "")[[1]]
    n_wild <- sum(splitstring == "?")

    if (n_wild == 0) {
      return(x)
    }

    variations <- perm(rep(1, n_wild))
    apply(variations, 1, function(j) {
        z <- splitstring
        z[z == "?"] <- j
        paste0(z, collapse = "")
    })
}

#' Update causal types based on nodal types
#' @inheritParams CausalQueries_internal_inherit_params
#' @param restrict_given a character vector of subsetting instructions for
#'   rows to be dropped from causal types data.frame.
#' @return A \code{data.frame} containing updated causal types in a model
#' @keywords internal
#' @examples
#' CausalQueries:::update_causal_types(make_model('X->Y'))

update_causal_types <- function(model, restrict_given = NULL) {
  # Remove var name prefix from nodal types (Y00 -> 00)
  # possible_types <- lapply(nodes, function(v)
  # gsub(v, '', possible_types[[v]])) names(possible_types) <- nodes

  # Get types as the combination of nodal types/possible_data. for
  # X->Y: X0Y00, X1Y00, X0Y10, X1Y10...
  df <- data.frame(
    expand.grid(get_nodal_types(model), stringsAsFactors = FALSE)
  )

  if (!is.null(restrict_given)) {
    to_keep <- vapply(restrict_given, function(i) {
      with(df, eval(parse(text = i)))
    }, logical(nrow(df)))

    to_keep <- !as.logical(apply(to_keep, 1, sum))
    df <- df[to_keep, ]
  }

  # Add names
  cnames <- causal_type_names(df)
  rownames(df) <- do.call(paste, c(cnames, sep = "."))
  class(df) <- c("causal_types", "data.frame")
  return(df)
}

