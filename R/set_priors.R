#' Make Priors
#'
#' A flexible function to generate priors for a model.
#'
#' Seven arguments govern *which* parameters should be altered. The default is 'all' but this can be reduced by specifying
#'
#' * \code{label} or \code{nodal_type} The label of a particular nodal type, written either in the form Y0000 or Y.Y0000
#'
#' * \code{node}, which restricts for example to parameters associated with node 'X'
#'
#' * \code{statement}, which restricts for example to nodal types that satisfy the statment 'Y[X=1] > Y[X=0]'
#'
#' * \code{confound}, which restricts for example to nodal types that satisfy the statment 'Y[X=1] > Y[X=0]'
#'
#' * \code{param_set}, which us useful when setting confound statements that produces several sets of parameters
#'
#' * \code{param_names}, which restricts in specific parameters by naming them
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
#' @param node A string (or list of strings) indicating nodes for which priors are to be altered
#' @param label String. Label for nodal type indicating nodal types for which priors are to be altered
#' @param statement A causal query (or list of queries) that determines nodal types for which priors are to be altered
#' @param confound A confound named list that restricts nodal types for which priors are to be altered. Adjustments are limited to nodes in the named list.
#' @param nodal_type String. Label for nodal type indicating nodal types for which priors are to be altered
#' @param param_set String. Indicates the name of the set of parameters to be modified (useful when setting confounds)
#' @param param_names String. The name of specific parameter in the form of, for example, 'X.1', 'Y.01'
#'
#' For instance \code{confound = list(X  = Y[X=1]> Y[X=0])} adjust parameters on X that are conditional on nodal types for Y.
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
#' # Passing a confound statement
#' model <- make_model('X->Y') %>%
#'  set_confound(list(X = 'Y[X=1] > Y[X=0]', X = 'Y[X=1] < Y[X=0]'))
#'
#' make_priors(model,
#'             confound = list(X='Y[X=1] > Y[X=0]',
#'                             X='Y[X=1] < Y[X=0]'),
#'             alphas = c(3, 6))
#'
#' make_priors(model, confound= list(X='Y[X=1] > Y[X=0]'), alphas = 4)
#' make_priors(model, param_set='X_1', alphas = 5)
#' make_priors(model, param_names='X_2.1', alphas = .75)
#'
#' make_model('X -> Y') %>%
#'   set_confound(list(X = 'Y[X=1]>Y[X=0]'))%>%
#'   make_priors(statement = 'X[]==1',
#'               confound = list(X = 'Y[X=1]>Y[X=0]', X = 'Y[X=1]<Y[X=0]'),
#'               alphas = c(2, .5))


make_priors <-
    function(model, alphas = NA, distribution = NA, node = NA, label = NA, statement = NA,
             confound = NA, nodal_type = NA, param_names = NA, param_set = NA) {

        make_par_values_multiple(
            model, y=get_priors(model), x = alphas, distribution = distribution,
            node = node, label = label, statement = statement,
            confound = confound, nodal_type = nodal_type, param_names = param_names,
            param_set = param_set, normalize = FALSE)
    }



#' Set prior distribution
#'
#' A flexible function to add priors to a model.
#'
#' Four arguments govern *which* parameters should be altered. The default is 'all' but this can be reduced by specifying
#'
#' * \code{label} The label of a particular nodal type, written either in the form Y0000 or Y.Y0000
#'
#' * \code{node}, which restricts for example to parameters associated with node 'X'
#'
#' * \code{statement}, which restricts for example to nodal types that satisfy the statment 'Y[X=1] > Y[X=0]'
#'
#' * \code{confound}, which restricts for example to nodal types that satisfy the statment 'Y[X=1] > Y[X=0]'
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
#' @param priors A optional vector of positive reals indicating priors over all parameters. These are interepreted as arguments for Dirichlet distributions---one for each parameter set. To see the structure of parameter sets examine model$parameters_df
#' @param distribution String (or list of strings) indicating a common prior distribution (uniform, jeffreys or certainty)
#' @param alphas Real positive numbers giving hyperparameters of the Dirichlet distribution
#' @param node A string (or list of strings) indicating nodes for which priors are to be altered
#' @param label String. Label for nodal type indicating nodal types for which priors are to be altered
#' @param statement A causal query (or list of queries) that determines nodal types for which priors are to be altered
#' @param confound A confound statement (or list of statements) that restricts nodal types for which priors are to be altered
#' @param nodal_type String. Label for nodal type indicating nodal types for which priors are to be altered
#' @param param_set String. Indicates the name of the set of parameters to be modified (useful when setting confounds)
#' @param param_names String. The name of specific parameter in the form of, for example, 'X.1', 'Y.01'
#'
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


set_priors <- function(model, priors = NULL, distribution = NA, alphas = NA, node = NA, label = NA,
    statement = NA, confound = NA, nodal_type = NA, param_names = NA, param_set = NA) {

    if (is.null(priors))
        priors <- make_priors(model, distribution = distribution, alphas = alphas, node = node, label = label,
            statement = statement, confound = confound, nodal_type = nodal_type, param_names = param_names,
            param_set = param_set)

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
#'
#' @export
#' @family priors
#' @examples
#' get_priors(make_model('X -> Y'))

get_priors <- function(model) {

    x <- model$parameters_df$priors
    names(x) <- model$parameters_df$param_names
    x

}




