#' Setting parameters
#'
#' Functionality for altering parameters:
#'
#' @param param_type A character. String specifying type of parameters to make ("flat", "prior_mean", "posterior_mean", "prior_draw", "posterior_draw", "define). With param_type set to \code{define} use arguments to be passed to \code{make_priors}; otherwise \code{flat} sets equal probabilities on each nodal type in each parameter set; \code{prior_mean}, \code{prior_draw}, \code{posterior_mean}, \code{posterior_draw} take parameters as the means or as draws from the prior or posterior.
#' @param warning Logical. Whether to warn about parameter renormalization.
#' @param ... Options passed onto \code{\link{make_priors}}.
#' @inheritParams CausalQueries_internal_inherit_params
#'
#' @name parameter_setting
NULL
#> NULL





#' Make a 'true' parameter vector
#'
#' A vector of 'true' parameters; possibly drawn from prior or posterior.
#'
#' @rdname parameter_setting
#'
#' @param normalize Logical. If parameter given for a subset of a family the residual elements are normalized so that parameters in param_set sum to 1 and provided params are unaltered.
#'
#' @return A vector of draws from the prior or distribution of parameters
#' @importFrom rstan extract
#' @export
#' @family parameters
#' @examples
#'
#' # make_parameters examples:
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
#'
#'\donttest{
#'
#' #altering values using \code{alter_at}
#' make_model("X -> Y") %>% make_parameters(parameters = c(0.5,0.25),
#' alter_at = "node == 'Y' & nodal_type %in% c('00','01')")
#'
#' #altering values using \code{param_names}
#' make_model("X -> Y") %>% make_parameters(parameters = c(0.5,0.25),
#' param_names = c("Y.10","Y.01"))
#'
#' #altering values using \code{statement}
#' make_model("X -> Y") %>% make_parameters(parameters = c(0.5),
#' statement = "Y[X=1] > Y[X=0]")
#'
#' #altering values using a combination of other arguments
#' make_model("X -> Y") %>% make_parameters(parameters = c(0.5,0.25),
#' node = "Y", nodal_type = c("00","01"))
#'
#' # Normalize renormalizes values not set so that value set is not renomalized
#' make_parameters(make_model('X -> Y'),
#'                statement = 'Y[X=1]>Y[X=0]', parameters = .5)
#' make_parameters(make_model('X -> Y'),
#'                statement = 'Y[X=1]>Y[X=0]', parameters = .5, normalize = FALSE)
#'
#'   }

make_parameters <- function(model, parameters = NULL, param_type = NULL, warning = TRUE, normalize = TRUE, ...) {

  is_a_model(model)

  if(!is.null(parameters) && (length(parameters) == length(get_parameters(model)))){

    out <- clean_param_vector(model, parameters)

  } else {

    if (!is.null(param_type)){
      if (!(param_type %in% c("flat", "prior_mean", "posterior_mean", "prior_draw", "posterior_draw", "define"))){
        stop("param_type should be one of `flat`, `prior_mean`, `posterior_mean`, `prior_draw`, `posterior_draw`, or `define`")
      }
    }


    # Figure out if we need to use make_par_values
    par_args = list(...)

    par_args_provided <- sum(names(par_args) %in% c("distribution", "alter_at", "node", "nodal_type", "label", "param_set", "given", "statement", "join_by", "param_names"))

    if (par_args_provided > 0 & is.null(param_type)){
      param_type <- "define"
    }

    if (is.null(param_type)){
      param_type <- "prior_mean"
    }

    # New (from parameters)
    if (param_type == "define"){
      param_value <- make_par_values(model,
                                     alter = "param_value",
                                     x = parameters,
                                     normalize = normalize,
                                     ...)
    }

    # Flat lambda
    if (param_type == "flat"){
      param_value <- make_priors(model, distribution = "uniform")
    }

    # Prior mean
    if (param_type == "prior_mean"){
      param_value <- get_priors(model)
    }

    # Prior draw
    if (param_type == "prior_draw"){
      param_value <- make_prior_distribution(model, 1)
    }

    # Posterior mean
    if (param_type == "posterior_mean"){

      if(is.null(model$posterior)){
        stop("Posterior distribution required")
      }

      param_value <- apply(model$posterior_distribution, 2, mean)
    }

    # Posterior draw
    if (param_type == "posterior_draw") {

      if (is.null(model$posterior)){
        stop("Posterior distribution required")
      }

      df <- model$posterior_distribution
      param_value <- df[sample(nrow(df), 1), ]
    }

    out <- clean_param_vector(model, param_value)

  }

  return(out)
}



#' Set parameters
#'
#' Add a true parameter vector to a model. Parameters can be created using arguments passed to
#' \code{\link{make_parameters}} and \code{\link{make_priors}}.
#'
#' @rdname parameter_setting
#'
#' @return An object of class \code{causal_model}. It essentially returns a list containing the elements comprising
#' a model (e.g. 'statement', 'nodal_types' and 'DAG') with true vector of parameters attached to it.
#' @export
#' @family parameters
#' @examples
#'
#' # set_parameters examples:
#'
#' make_model('X->Y') %>% set_parameters(1:6) %>% get_parameters()
#'
#' # Simple examples
#' model <- make_model('X -> Y')
#' data  <- simulate_data(model, n = 2)
#' model <- update_model(model, data)
#' set_parameters(model, parameters = c(.25, .75, 1.25,.25, .25, .25))
#' set_parameters(model, param_type = 'flat')
#' set_parameters(model, param_type = 'prior_draw')
#' set_parameters(model, param_type = 'prior_mean')
#' set_parameters(model, param_type = 'posterior_draw')
#' set_parameters(model, param_type = 'posterior_mean')
#'
#'
#'\donttest{
#'
#' #altering values using \code{alter_at}
#' make_model("X -> Y") %>% set_parameters(parameters = c(0.5,0.25),
#' alter_at = "node == 'Y' & nodal_type %in% c('00','01')")
#'
#' #altering values using \code{param_names}
#' make_model("X -> Y") %>% set_parameters(parameters = c(0.5,0.25),
#' param_names = c("Y.10","Y.01"))
#'
#' #altering values using \code{statement}
#' make_model("X -> Y") %>% set_parameters(parameters = c(0.5),
#' statement = "Y[X=1] > Y[X=0]")
#'
#' #altering values using a combination of other arguments
#' make_model("X -> Y") %>% set_parameters(parameters = c(0.5,0.25),
#' node = "Y", nodal_type = c("00","01"))
#'
#'
#'   }

set_parameters <- function(model, parameters = NULL, param_type = NULL, warning = FALSE, ...) {

  # parameters are created unless a vector of full length is provided
  if (length(parameters) != length(get_parameters(model))) {

    if(!is.null(parameters)){
      parameters <- make_parameters(model, parameters = parameters, param_type = "define", ...)
    }

    if(is.null(parameters)){
      parameters <- make_parameters(model, param_type = param_type, ...)
    }

  }

  model$parameters_df$param_value <- parameters
  model$parameters_df <- clean_params(model$parameters_df, warning = warning)

  return(model)

}


#' Get parameters
#'
#' Extracts parameters as a named vector
#'
#' @rdname parameter_setting
#'
#' @return A vector of draws from the prior or distribution of parameters
#' @importFrom gtools rdirichlet
#' @family parameters
#' @export
#' @examples
#'
#' # get_parameters examples:
#'
#' get_parameters(make_model('X -> Y'))

get_parameters <- function(model, param_type = NULL) {

  if (is.null(param_type)) {
    x <- model$parameters_df$param_value
    names(x) <- model$parameters_df$param_names
  }

  if (!is.null(param_type)){
    x <- make_parameters(model, param_type = param_type)
  }

  return(x)

}

