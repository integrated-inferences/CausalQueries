#' Make a 'true' parameter vector
#'
#' A vector of 'true' parameters; possibly drawn from prior or posterior.
#'
#' @inheritParams CausalQueries_internal_inherit_params
#' @param param_type A character. String specifying type of parameters to make ("flat", "prior_mean", "posterior_mean", "prior_draw", "posterior_draw", "define). With param_type set to \code{define} use arguments to be passed to \code{make_priors}; otherwise \code{flat} sets equal probabilities on each nodal type in each parameter set; \code{prior_mean}, \code{prior_draw}, \code{posterior_mean}, \code{posterior_draw} take parameters as the means or as draws from the prior or posterior.
#' @param warning Logical. Whether to warn about parameter renormalization.
#' @param normalize Logical. If parameter given for a subset of a family the residual elements are normalized so that parameters in param_set sum to 1 and provided params are unaltered.
#' @param ... Options passed onto \code{\link{make_priors}}.
#' @return A vector of draws from the prior or distribution of parameters
#' @importFrom rstan extract
#' @export
#' @family parameters
#' @examples
#'
#' # Simple examples
#' model <- make_model('X -> Y')
#' data  <- simulate_data(model, n = 2)
#' model <- update_model(model, data)
#' make_parameters(model, parameters = c(.25, .75, 1.25,.25, .25, .25))
#' make_parameters(model, param_type = 'flat')
#' make_parameters(model, param_type = 'prior_draw')
#' make_parameters(model, param_type = 'prior_mean')
#' make_parameters(model, param_type = 'posterior_draw')
#' make_parameters(model, param_type = 'posterior_mean')
#'
#' # Harder examples, using \code{define} and priors arguments to define
#' # specific parameters using causal syntax
#'
#'# Using labels: Two values for two nodes with the same label
#' make_model('X -> M -> Y') %>% make_parameters(label = "01", parameters = c(0,1))
#'\donttest{
#' # Using statement:
#' make_model('X -> Y') %>%
#'    make_parameters(statement = c('Y[X=1]==Y[X=0]'), parameters = c(.2,0))
#' make_model('X -> Y') %>%
#'    make_parameters(statement = c('Y[X=1]>Y[X=0]', 'Y[X=1]<Y[X=0]'), parameters = c(.2,0))
#'
#' # Normalize renormalizes values not set so that value set is not renomalized
#' make_parameters(make_model('X -> Y'),
#'                statement = 'Y[X=1]>Y[X=0]', parameters = .5)
#' make_parameters(make_model('X -> Y'),
#'                statement = 'Y[X=1]>Y[X=0]', parameters = .5, normalize = FALSE)
#'
#' # May be built up
#' make_model('X -> Y') %>%
#'   set_confound(list(X = 'Y[X=1]>Y[X=0]'))  %>%
#'   set_parameters(confound   = list(X='Y[X=1]>Y[X=0]', X='Y[X=1]<=Y[X=0]'),
#'                  parameters = list(c(.2, .8), c(.8, .2))) %>%
#'   set_parameters(statement  = 'Y[X=1]>Y[X=0]', parameters = .5) %>%
#'   get_parameters
#'   }

make_parameters <- function(model, parameters = NULL, param_type = NULL, warning = TRUE, normalize = TRUE, ...) {

    is_a_model(model)
    if (!is.null(parameters) && (length(parameters) == length(get_parameters(model))))
        return(clean_param_vector(model, parameters))

    if (!is.null(param_type))
        if (!(param_type %in% c("flat", "prior_mean", "posterior_mean", "prior_draw", "posterior_draw",
            "define"))) {
            stop("param_type should be one of `flat`, `prior_mean`, `posterior_mean`, `prior_draw`, `posterior_draw`, or `define`")
        }

    # Figure out if we need to use make_par_values
    par_args = list(...)

    par_args_provided <- sum(names(par_args) %in% c("distribution", "parameters", "node", "label", "statement",
        "confound", "nodal_type", "param_set", "param_names"))
    if (par_args_provided > 0 & is.null(param_type))
        param_type <- "define"

    if (is.null(param_type))
        param_type <- "prior_mean"


    # Magic

    # New (from parameters)
    if (param_type == "define") {
        param_value <- make_par_values_multiple(model,
                                                y = get_parameters(model),
                                                x = parameters,
                                                normalize = normalize,
                                                ...)
    }

    # Flat lambda
    if (param_type == "flat") {
        param_value <- make_priors(model, distribution = "uniform")
    }

    # Prior mean
    if (param_type == "prior_mean") {
        param_value <- get_priors(model)
    }

    # Prior draw
    if (param_type == "prior_draw") {
        param_value <- make_prior_distribution(model, 1)
    }

    # Posterior mean
    if (param_type == "posterior_mean") {
        if (is.null(model$posterior))
            stop("Posterior distribution required")
        param_value <- apply(model$posterior_distribution, 2, mean)
    }

    # Posterior draw
    if (param_type == "posterior_draw") {
        if (is.null(model$posterior))
            stop("Posterior distribution required")
        df <- model$posterior_distribution
        param_value <- df[sample(nrow(df), 1), ]
    }

    # Clean: Check normalization, using data families
    clean_param_vector(model, param_value)

}



#' Set parameters
#'
#' Add a true parameter vector to a model. Parameters can be created using arguments passed to
#' \code{\link{make_parameters}} and \code{\link{make_priors}}.
#'
#' Argument 'param_type' is passed to make_priors and specifies  one of 'flat', 'prior_mean',
#' 'posterior_mean', 'prior_draw', 'posterior_draw', and 'define'.
#' With param_type set to \code{define} use arguments to be passed to \code{make_priors}; otherwise
#' \code{flat} sets equal probabilities on each nodal param_type in each parameter set;
#' \code{prior_mean}, \code{prior_draw}, \code{posterior_mean}, \code{posterior_draw}
#' take parameters as the means or as draws from the prior or posterior.
#'
#' @inheritParams CausalQueries_internal_inherit_params
#' @param param_type A character. String specifying type of parameters to set ('flat', 'prior_mean', 'posterior_mean', 'prior_draw', 'posterior_draw', 'define). With param_type set to \code{define} use arguments to be passed to \code{make_priors}; otherwise \code{flat} sets equal probabilities on each nodal param_type in each parameter set; \code{prior_mean}, \code{prior_draw}, \code{posterior_mean}, \code{posterior_draw} take parameters as the means or as draws from the prior or posterior.
#' @param warning Logical. Whether to warn about parameter renormalization
#' @param ... Arguments to be passed to make_parameters
#' @return An object of class \code{causal_model}. It essentially returns a list containing the elements comprising
#' a model (e.g. 'statement', 'nodal_types' and 'DAG') with true vector of parameters attached to it.
#' @export
#' @family parameters
#' @examples
#' make_model('X->Y') %>% set_parameters(1:6) %>% get_parameters()
#'
#' make_model('X -> Y') %>%
#'   set_confound(list(X = 'Y[X=1]>Y[X=0]'))  %>%
#'   set_parameters(confound = list(X='Y[X=1]>Y[X=0]', X='Y[X=1]<=Y[X=0]'),
#'                  parameters = list(c(.2, .8), c(.8, .2))) %>%
#'   set_parameters(statement = 'Y[X=1]>Y[X=0]', parameters = .5) %>%
#'   get_parameters

set_parameters <- function(model, parameters = NULL, param_type = NULL, warning = FALSE, ...) {

    # parameters are created unless a vector of full length is provided
    if (length(parameters) != length(get_parameters(model))) {
        if(!is.null(parameters)) parameters <- make_parameters(model, parameters = parameters, param_type = "define", ...)
        if(is.null(parameters))  parameters <- make_parameters(model, param_type = param_type, ...)
    }

    model$parameters_df$param_value <- parameters
    model$parameters_df <- clean_params(model$parameters_df, warning = warning)

    model

}


#' Get parameters
#'
#' Extracts parameters as a named vector
#'
#' @inheritParams CausalQueries_internal_inherit_params
#' @param param_type A character. String specifying type of parameters to set ('flat', 'prior_mean', 'posterior_mean', 'prior_draw', 'posterior_draw', 'define'). With param_type set to \code{define} use arguments to be passed to \code{make_priors}; otherwise \code{flat} sets equal probabilities on each nodal param_type in each parameter set; \code{prior_mean}, \code{prior_draw}, \code{posterior_mean}, \code{posterior_draw} take parameters as the means or as draws from the prior or posterior.
#' @return A vector of draws from the prior or distribution of parameters
#' @importFrom gtools rdirichlet
#' @family parameters
#' @export
#' @examples
#' get_parameters(make_model('X -> Y'))

get_parameters <- function(model, param_type = NULL) {

    if (is.null(param_type)) {
        x <- model$parameters_df$param_value
        names(x) <- model$parameters_df$param_names
    }

    if (!is.null(param_type))
        x <- make_parameters(model, param_type = param_type)

    x

}
