#' Setting priors
#'
#' Functionality for altering priors:
#'
#' Seven arguments govern which parameters should be altered. The default is
#' 'all' but this can be reduced by specifying
#'
#' * \code{alter_at} String specifying filtering operations to be applied to
#'   parameters_df, yielding a logical vector indicating parameters for which
#'   values should be altered. "node == 'X' & nodal_type %in% c('00','01')"
#'
#' * \code{node}, which restricts for example to parameters associated with node
#'   'X'
#'
#' * \code{label} or \code{nodal_type} The label of a particular nodal type,
#'   written either in the form Y0000 or Y.Y0000
#'
#' * \code{param_set} The param_set of a parameter.
#'
#' * \code{given} Given parameter set of a parameter.
#'
#' * \code{statement}, which restricts for example to nodal types that satisfy
#'   the statement 'Y[X=1] > Y[X=0]'
#'
#' * \code{param_set}, \code{given}, which are useful when setting confound
#'   statements that produce several sets of parameters
#'
#' Two arguments govern what values to apply:
#'
#' * \code{alphas} is one or more non-negative numbers and
#'
#' * \code{distribution} indicates one of a common class: uniform, Jeffreys, or
#'   'certain'
#'
#' Forbidden statements include:
#' \itemize{
#'   \item Setting \code{distribution} and \code{values} at the same time.
#'   \item Setting a \code{distribution} other than uniform, Jeffreys, or
#'     certainty.
#'   \item Setting negative values.
#'   \item specifying \code{alter_at} with any of \code{node},
#'     \code{nodal_type}, \code{param_set}, \code{given}, \code{statement}, or
#'     \code{param_names}
#'   \item specifying \code{param_names} with any of \code{node},
#'     \code{nodal_type}, \code{param_set}, \code{given}, \code{statement}, or
#'     \code{alter_at}
#'   \item specifying \code{statement} with any of \code{node} or
#'     \code{nodal_type}
#' }
#'
#'
#' @name prior_setting
NULL
#> NULL


#' Make Priors
#'
#' \code{make_priors} Generates priors for a model.
#'
#' @rdname prior_setting
#' @param alphas Real positive numbers giving parameters of a
#'   Dirichlet prior distribution ("hyperparameters")
#' @inheritParams make_par_values
#'
#' @return A vector indicating the parameters of the prior distribution
#'   of the nodal types ("hyperparameters").
#'
#' @family priors
#' @export
#'
#' @examples
#'
#' # make_priors examples:
#'
#' # Pass all nodal types
#' model <- make_model("Y <- X")
#' make_priors(model, alphas = .4)
#' make_priors(model, distribution = "jeffreys")
#'
#' model <- CausalQueries::make_model("X -> M -> Y; X <-> Y")
#'
#' #altering values using \code{alter_at}
#' make_priors(model = model, alphas = c(0.5,0.25),
#' alter_at = "node == 'Y' & nodal_type %in% c('00','01') & given == 'X.0'")
#'
#' #altering values using \code{param_names}
#' make_priors(model = model, alphas = c(0.5,0.25),
#' param_names = c("Y.10_X.0","Y.10_X.1"))
#'
#' #altering values using \code{statement}
#' make_priors(model = model, alphas = c(0.5,0.25),
#' statement = "Y[M=1] > Y[M=0]")
#'
#' #altering values using a combination of other arguments
#' make_priors(model = model, alphas = c(0.5,0.25),
#' node = "Y", nodal_type = c("00","01"), given = "X.0")

make_priors <- function(model,
                        alphas = NA,
                        distribution = NA,
                        alter_at = NA,
                        node = NA,
                        nodal_type = NA,
                        label = NA,
                        param_set = NA,
                        given = NA,
                        statement = NA,
                        join_by = "|",
                        param_names = NA){

  if (all(!is.na(c(nodal_type, label)))) {
    stop("cannot define both nodal_type and label simultaniously")
  }

  if (!is.na(label)) {
    warning("label is depreciated, use nodal_type instead")
    nodal_type <- label
  }

  out <- make_par_values(
    model = model,
    alter = "priors",
    x = alphas,
    alter_at = alter_at,
    node = node,
    nodal_type = nodal_type,
    param_set = param_set,
    given = given,
    statement = statement,
    join_by = join_by,
    param_names = param_names,
    distribution = distribution,
    normalize = FALSE
  )

  names(out) <- model$parameters_df$param_names
  return(out)
}


#' Set prior distribution
#'
#' \code{set_priors}  Adds priors to a model.
#'
#' @rdname prior_setting
#'
#' @param alphas Real positive numbers giving hyperparameters of
#'   the Dirichlet distribution
#' @inheritParams make_par_values
#' @return An object of class \code{causal_model}. It essentially returns a
#'   list containing the elements comprising a model
#'   (e.g. 'statement', 'nodal_types' and 'DAG') with the `priors` attached
#'   to it.
#' @export
#' @family priors
#' @examples
#'
#' # set_priors examples:
#'
#' # Pass all nodal types
#' model <- make_model("Y <- X")
#' set_priors(model, alphas = .4)
#' set_priors(model, distribution = "jeffreys")
#'
#' model <- CausalQueries::make_model("X -> M -> Y; X <-> Y")
#'
#' #altering values using \code{alter_at}
#' set_priors(model = model, alphas = c(0.5,0.25),
#' alter_at = "node == 'Y' & nodal_type %in% c('00','01') & given == 'X.0'")
#'
#' #altering values using \code{param_names}
#' set_priors(model = model, alphas = c(0.5,0.25),
#' param_names = c("Y.10_X.0","Y.10_X.1"))
#'
#' #altering values using \code{statement}
#' set_priors(model = model, alphas = c(0.5,0.25),
#' statement = "Y[M=1] > Y[M=0]")
#'
#' #altering values using a combination of other arguments
#' set_priors(model = model, alphas = c(0.5,0.25), node = "Y",
#' nodal_type = c("00","01"), given = "X.0")

set_priors <- function(model,
                       alphas = NA,
                       distribution = NA,
                       alter_at = NA,
                       node = NA,
                       nodal_type = NA,
                       label = NA,
                       param_set = NA,
                       given = NA,
                       statement = NA,
                       join_by = "|",
                       param_names = NA){

  priors <- make_priors(
    model = model,
    alphas = alphas,
    distribution = distribution,
    alter_at = alter_at,
    node = node,
    nodal_type = nodal_type,
    label = label,
    param_set = param_set,
    given = given,
    statement = statement,
    join_by = join_by,
    param_names = param_names
  )

  model$parameters_df$priors <- priors

  return(model)

}


#' Get priors
#'
#' Extracts priors as a named vector
#'
#' @rdname prior_setting
#'
#' @param model A model object generated by make_model().
#' @return A vector indicating the hyperparameters of the prior distribution
#'   of the nodal types.
#' @export
#' @family priors
#' @examples
#'
#' # get_priors examples:
#'
#' get_priors(make_model('X -> Y'))

get_priors <- function(model) {

    x <- model$parameters_df$priors
    names(x) <- model$parameters_df$param_names
    return(x)

}




