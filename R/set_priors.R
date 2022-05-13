#' Make Priors
#'
#' A flexible function to generate priors for a model.
#'
#' Six arguments govern *which* parameters should be altered. The default is 'all' but this can be reduced by specifying
#'
#' * \code{node}, which restricts for example to parameters associated with node 'X'
#'
#' * \code{nodal_type} (or \code{label}, deprecated) The label of a particular nodal type, written either in the form Y0000 or Y.Y0000
#'
#' * \code{param_names}, which restricts in specific parameters by naming them
#'
#' * \code{statement}, which restricts for example to nodal types that satisfy the statement 'Y[X=1] > Y[X=0]'
#'
#' * \code{param_set}, \code{given}, which are useful when setting confound statements that produces several sets of parameters
#'
#' Two arguments govern what values to apply:
#'
#' * \code{alphas} is one or more non negative numbers and
#'
#' * \code{distribution} indicates one of a common class: uniform, jeffreys, or 'certain'
#'
#' Any arguments entered as lists or vectors of size > 1 should be of the same length as each other.
#'
#' @param model A model created with \code{make_model}
#' @param alphas Real positive numbers giving hyperparameters of the Dirichlet distribution
#' @param distribution String (or list of strings) indicating a common prior distribution (uniform, jeffreys or certainty)
#' @param alter_at A string giving a logic statement using column names of `parameters_df` to indicate what changes to make.
#' @param node A string (or list of strings) indicating nodes for which priors are to be altered
#' @param label A string. Label for nodal type indicating nodal types for which priors are to be altered
#' @param nodal_type A string. Equivalent to label. Label for nodal type indicating nodal types for which priors are to be altered
#' @param statement A causal query (or list of queries) that determines nodal types for which priors are to be altered
#' @param given A string that restricts nodal types for which priors are to be altered. Adjustments are limited to nodes in the named list.
#' @param param_set A string. Indicates the name of the set of parameters to be modified (useful when setting confounds)
#' @param param_names A string. The name of specific parameter in the form of, for example, 'X.1', 'Y.01'
#' @return A vector indicating the hyperparameters of the prior distribution of the nodal types.
#'
#' @family priors
#' @export
#' @examples
#'
#' # Pass all nodal types
#' model <- make_model("Y <- X")
#' make_priors(model, alphas = .4)
#' make_priors(model, distribution = "jeffreys")
#'
#' # Passing by names of node, parameter set or label
#' model <- make_model('X -> M -> Y')
#' make_priors(model, param_name = "X.1", alphas = 2)
#' make_priors(model, node = 'X', alphas = 3)
#' make_priors(model, param_set = 'Y', alphas = 5)
#' make_priors(model, node = c('X', 'Y'), alphas = 3)
#' make_priors(model, param_set = c('X', 'Y'), alphas = 5)
#' make_priors(model, node = list('X', 'Y'), alphas = list(3, 6))
#' make_priors(model, param_set = list('X', 'Y'), alphas = list(4, 6))
#' make_priors(model, node = c('X', 'Y'), distribution = c('certainty', 'jeffreys'))
#' make_priors(model, param_set = c('X', 'Y'), distribution = c('jeffreys', 'certainty'))
#' make_priors(model, label = '01', alphas = 5)
#' make_priors(model, node = 'Y', label = '00', alphas = 2)
#' make_priors(model, node =c('M', 'Y'), label = '11', alphas = 4)
#'
#' # Passing a causal statement
#' make_priors(model, statement = 'Y[M=1] > Y[M=0]', alphas = 3)
#' make_priors(model, statement = c('Y[M=1] > Y[M=0]', 'M[X=1]== M[X=0]'), alphas = c(3, 2))
#'
#' # Models with confounding
#' model <- make_model('X -> Y; X <-> Y')
#' make_priors(model, node = "X", alphas = c(3, 6))
#' make_priors(model, given = "X.1", alphas = 4)
#' make_priors(model, param_set='Y.X.1', alphas = 5)
#' make_priors(model, alter_at = "nodal_type == '01'", 2)
#'

make_priors <-
    function(model,
             alphas = NA,
             distribution = NA,
             alter_at = NA,
             node = NA,
             nodal_type = NA,
             param_set = NA,
             given = NA,
             label = NA,
             statement = NA,
             param_names = NA
) {
    is_a_model(model)

      if(!is.na(label)) {
        nodal_type <- label
        message("label is deprecated; please use nodal_type")
      }
      make_par_values(
            model,
            alter = "priors",
            x = alphas,
            distribution = distribution,
            alter_at = alter_at,
            node = node,
            nodal_type = nodal_type,
            param_set = param_set,
            given = given,
            statement = statement,
            param_names = param_names,
            normalize = FALSE)
    }



#' Set prior distribution
#'
#' A flexible function to add priors to a model.
#'
#' Six arguments govern *which* parameters should be altered. The default is 'all' but this can be reduced by specifying
#'
#' * \code{node}, which restricts for example to parameters associated with node 'X'
#'
#' * \code{nodal_type} (or \code{label}, deprecated) The label of a particular nodal type, written either in the form Y0000 or Y.Y0000
#'
#' * \code{param_names}, which restricts in specific parameters by naming them
#'
#' * \code{statement}, which restricts for example to nodal types that satisfy the statement 'Y[X=1] > Y[X=0]'
#'
#' * \code{param_set}, \code{given}, which are useful when setting confound statements that produces several sets of parameters
#'
#' Two arguments govern what values to apply:
#'
#' * \code{alphas} is one or more non negative numbers and
#'
#' * \code{distribution} indicates one of a common class: uniform, jeffreys, or 'certain'
#'
#' Any arguments entered as lists or vectors of size > 1 should be of the same length as each other.
#'
#' For more examples and details see \code{make_priors}
#'
#' @param model A model created with \code{make_model}
#' @param priors A optional vector of positive reals indicating priors over all parameters. These are interpreted as arguments for Dirichlet distributions---one for each parameter set. To see the structure of parameter sets examine model$parameters_df
#' @param ... Arguments passed to \code{make_priors}
#' @return An object of class \code{causal_model}. It essentially returns a list containing the elements comprising
#' a model (e.g. 'statement', 'nodal_types' and 'DAG') with the `priors` attached to it.
#' @export
#' @family priors
#' @examples
#'
#' library(dplyr)
#' # Set priors to the model
#' model <- make_model('X -> Y') %>%
#'   set_priors(alphas = 3)
#' get_priors(model)
#' model <- make_model('X -> Y') %>%
#'   set_priors(distribution = 'jeffreys')
#' get_priors(model)
#'
#' # Pass all nodal types
#' model <- make_model("Y <- X") %>%
#'   set_priors(.4)
#' get_priors(model)
#' model <- make_model("Y <- X") %>%
#'   set_priors(.7)
#' get_priors(model)
#' model <- make_model("Y <- X") %>%
#'   set_priors(distribution = "jeffreys")
#' get_priors(model)
#'
#' # Passing by names of node, parameter set or label
#' model <- make_model('X -> M -> Y')
#' model_new_priors <- set_priors(model, param_name = "X.1", alphas = 2)
#' get_priors(model_new_priors)
#' model_new_priors <- set_priors(model, node = 'X', alphas = 3)
#' get_priors(model_new_priors)
#' model_new_priors <- set_priors(model, param_set = 'Y', alphas = 5)
#' get_priors(model_new_priors)
#' model_new_priors <- set_priors(model, node = c('X', 'Y'), alphas = 3)
#' get_priors(model_new_priors)
#' model_new_priors <- set_priors(model, param_set = c('X', 'Y'), alphas = 5)
#' get_priors(model_new_priors)
#' model_new_priors <- set_priors(model, node = list('X', 'Y'), alphas = list(3, 6))
#' get_priors(model_new_priors)
#' model_new_priors <- set_priors(model, param_set = list('X', 'Y'), alphas = list(4, 6))
#' get_priors(model_new_priors)
#' model_new_priors <- set_priors(model,
#'                                node = c('X', 'Y'),
#'                                distribution = c('certainty', 'jeffreys'))
#' get_priors(model_new_priors)
#' model_new_priors <- set_priors(model,
#'                                param_set = c('X', 'Y'),
#'                                distribution = c('jeffreys', 'certainty'))
#' get_priors(model_new_priors)
#' model_new_priors <- set_priors(model, label = '01', alphas = 5)
#' get_priors(model_new_priors)
#' model_new_priors <- set_priors(model, node = 'Y', label = '00', alphas = 2)
#' get_priors(model_new_priors)
#' model_new_priors <- set_priors(model, node =c('M', 'Y'), label = '11', alphas = 4)
#' get_priors(model_new_priors)
#'
#' # Passing a causal statement
#' model_new_priors <- set_priors(model, statement = 'Y[M=1] > Y[M=0]', alphas = 3)
#' get_priors(model_new_priors)
#' model_new_priors <- set_priors(model,
#'                                statement = c('Y[M=1] > Y[M=0]', 'M[X=1]== M[X=0]'),
#'                                alphas = c(3, 2))
#' get_priors(model_new_priors)
#'
#' # Passing a confound statement
#' model <- make_model('X->Y') %>%
#'  set_confound(list(X = 'Y[X=1] > Y[X=0]', X = 'Y[X=1] < Y[X=0]'))
#'
#' model_new_priors <- set_priors(model,
#'             confound = list(X='Y[X=1] > Y[X=0]',
#'                             X='Y[X=1] < Y[X=0]'),
#'             alphas = c(3, 6))
#' get_priors(model_new_priors)
#' model_new_priors <- set_priors(model, confound= list(X='Y[X=1] > Y[X=0]'), alphas = 4)
#' get_priors(model_new_priors)
#' model_new_priors <- set_priors(model, param_set='X_1', alphas = 5)
#' get_priors(model_new_priors)
#' model_new_priors <-  set_priors(model, param_names='X_2.1', alphas = .75)
#' get_priors(model_new_priors)
#'
#' # A more complex example
#' model <- make_model('X -> Y') %>%
#'   set_confound(list(X = 'Y[X=1]>Y[X=0]'))%>%
#'   set_priors(statement = 'X[]==1',
#'               confound = list(X = 'Y[X=1]>Y[X=0]', X = 'Y[X=1]<Y[X=0]'),
#'               alphas = c(2, .5))
#' get_priors(model)

set_priors <- function(model, priors = NULL, ...) {

    is_a_model(model)
    if (is.null(priors))
        priors <- make_priors(model, ...)

    if (!is.null(priors) && isTRUE(!is.numeric(priors) | !all(priors >= 0)))
        stop("Argument priors must be a vector of non negative real numbers")

    model$parameters_df$priors <- priors

    model

}


#' Get priors
#'
#' Extracts priors as a named vector
#'
#' @param model A model object generated by make_model().
#' @return A vector indicating the hyperparameters of the prior distribution of the nodal types.
#' @export
#' @family priors
#' @examples
#' get_priors(make_model('X -> Y'))

get_priors <- function(model) {

    x <- model$parameters_df$priors
    names(x) <- model$parameters_df$param_names
    x

}




