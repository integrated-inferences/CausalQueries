#' make_priors2
#'
#' This function creates the priors to be passed on nodal types with \code{set_priors}
#'
#' Forbidden statements include:
#' \itemize{
#'   \item Setting \code{prior_distribution} and \code{alphas} at the same time.
#'   \item Setting a \code{prior_distribution} other than uniform, jeffreys or certainty.
#'   \item Declaring two of these arguments at the same time: \code{statement}, \code{confound} and \code{label}.
#'   \item Setting negative priors.
#' }
#'
#' \code{parameter_set} may be passed along with either \code{statement} or \code{confound} but not with \code{label}.
#' In either case, the parameter_set should refer to the same node as the query or the confound statement.
#' Function takes as arguments the following:
#'
#' @param model A mode created with \code{make_model}
#' @param parameter_set A node
#' @param statement A query
#' @param confound A confound statement
#' @param label The name of a nodal type
#' @param prior_distribution A common prior distribution (uniform, jeffreys or certainty)
#' @param alphas Hyperparameters of the Dirichlet distribution
#'
#' @export
#' @examples
#' model <- make_model("X -> M -> Y; X->Y")
#'
#' make_priors2(model, prior_distribution = "jeffreys")
#' make_priors2(model, alphas = 111)
#'
#' make_priors2(model, statement = "(Y[X=0, M = .] > Y[X=1, M = 0])", alphas = "666")
#' make_priors2(model, parameter_set = "Y", statement = "(Y[X=0, M = .] > Y[X=1, M = 0])", alphas = 777)
#'
#' make_priors2(model, parameter_set = "M", alphas = 888)
#'
#' make_priors2(model, label = "X0", alphas = 999)
#'
#' model <- make_model("X->Y")
#' model <- set_confound(model, list(X = "Y[X=1] > Y[X=0]"))
#' model <- set_confound(model, list(X = "Y[X=1] < Y[X=0]"))
#' make_priors2(model, confound = "Y[X=1] > Y[X=0]", alphas = 3)
#' make_priors2(model, parameter_set = "X", confound = "Y[X=1] > Y[X=0]", alphas = 3)


make_priors2 <- function(model,
                         prior_distribution=NA,
                         parameter_set=NA,
                         statement=NA,
                         confound=NA,
                         alphas=NULL,
                         label=NA)
{
  #1. House keeping
  #1.1. No prior distribution and alphas at the same time
  if (!is.na(prior_distribution) & !is.null(alphas))
    stop("alphas and prior_distribution cannot be declared at the same time. try sequentially")

  #1.2. Prior distribution different from "uniform, jeffreys or certainty"
  if (!is.na(prior_distribution) & !(prior_distribution %in% c("uniform", "jeffreys", "certainty")))
    stop("prior_distribution should be either 'uniform', 'jeffreys', or 'certainty'.")

  #1.3. Various statements at the same time
  if (!is.na(confound) & !is.na(statement) | !is.na(confound) & !is.na(label) | !is.na(statement) & !is.na(label))
    stop("too many statements. try setting priors sequentially 1")

  #1.4. Alphas negatives
  if (!is.null(alphas)) {if(alphas < 0) stop("alphas must be non-negative")}

  #Choose case
  if(!is.na(prior_distribution))
    v <- switch (prior_distribution, "uniform" = 1, "jeffreys" = .5, "certainty" = 10000)

  if(!is.null(alphas))
    v <- alphas

  #Create parameter matrix if not there and help variables
  if(is.null(model$P)) {model  <- set_parameter_matrix(model)}
  P          <- get_parameter_matrix(model)
  par_names  <- get_parameter_names(model = model)
  n_params   <- length(par_names)

  #2. Default uniform distributions
  if (is.na(prior_distribution) & is.null(alphas))
  {

    if(!(is.na(parameter_set) & is.na(confound) & is.na(statement) & is.na(label)))
      stop("prior_distribution or alphas should be explicitly stated")

    return(rep(1, n_params))
  }

  #3. Common priors (jeffreys, uniform, certainty)
  if (!is.na(prior_distribution) & is.na(parameter_set) & is.na(statement) & is.na(confound) & is.null(alphas) & is.na(label))
    return(rep(v, n_params))

  #4. Set alphas (could be one value, vector of any length)
  if (!is.null(alphas) & is.na(confound) & is.na(prior_distribution) & is.na(parameter_set) & is.na(statement) & is.na(label))
    return(rep(v, length.out= n_params))

  #5. Set priors given a statement
  #5.1. Set priors for nodal types of the query with the value of alpha
  if (!is.na(statement) & is.na(parameter_set) & is.na(confound) & is.na(label))
  {
    l_types <- lookup_type(model, statement)
    node    <- l_types$node
    t_types <- Filter(function(types_in_statement) types_in_statement == TRUE, l_types$types)
    names_types <- names(t_types)
    to_alter <- paste(node,names_types, sep = ".")
    return(rep(v, length.out=length(to_alter)))
  }


  #5.2. Set priors for nodal types of query, if at the same time parameter_set is specified
  if (!is.na(statement) & !is.na(parameter_set) & is.na(confound) & is.na(label))
  {
    l_types <- lookup_type(model, query=statement)
    node <- l_types$node
    t_types <- Filter(function(types_in_statement) types_in_statement == TRUE, l_types$types)
    names_types <- names(t_types)
    to_alter <- paste(node,names_types, sep = ".")
    if (node != parameter_set)
    {
      stop("statement and parameter set refer to different node")
    }else
      return(rep(v, length.out=length(to_alter)))
  }

  #6. Set priors given a parameter set
  if (!is.na(parameter_set) & is.na(statement) & is.na(confound) & is.na(label))
  {
  all_nodes <- get_nodal_types(model)
  my_node <- all_nodes[[parameter_set]]
  if (!is.null(my_node))
  {
    to_alter <- paste(parameter_set,my_node, sep = ".")
    return(rep(v, length.out=length(to_alter)))
  }else
    stop("parameter is not included in model")
  }

  #7. Set priors given a confound statement
  #7.1. Set priors with confound and explicit parameter set
  if (!is.na(confound) & !is.na(parameter_set))
  {
    param_family <- attr(model$P, "param_family")
    param_set    <- attr(model$P, "param_set")
    P_short <- P[, get_types(model, confound)$types, drop=FALSE]
    nom <- row.names(P_short)
    t <- paste(param_set, nom, sep = ".")
    to_alter <- t[apply(P_short, 1, sum) != 0 & param_family == parameter_set]
    return(rep(v, length.out=length(to_alter)))
  }

  #7.2. Set priors with confound statement
  if (!is.na(confound) & is.na(parameter_set) & is.na(label))
  {
    param_family <- attr(model$P, "param_family")
    param_set    <- attr(model$P, "param_set")
    P_short <- P[, get_types(model, confound)$types, drop=FALSE]
    nom <- row.names(P_short)
    t <- paste(param_set, nom, sep = ".")
    to_alter <- t[apply(P_short, 1, sum) != 0]
    return(rep(v, length.out=length(to_alter)))
  }

  #8. Set priors with labels
  if (!is.na(label) & is.na(statement) & is.na(confound))
  {
    if (!is.na(parameter_set))
    {
      stop("try sequentially setting param set and label")
    }else
    param <- get_parameter_names(P, model)
    regular_expression <- paste0("\\b", label, "\\b")
    to_alter <- param[grepl(regular_expression, param)==TRUE]
    if (length(to_alter)==0)
    {
      stop("label does not correspond to any parameter in model")
    }else
    {
      return(rep(v, length.out=length(to_alter)))
    }
  }
}






