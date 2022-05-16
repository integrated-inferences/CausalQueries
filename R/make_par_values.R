#' make_par_values
#'
#' This is the one step function for make_priors and make_parameters.
#' See \code{make_priors} for more help.
#'
#' @param model model created with \code{make_model}
#' @param alter character vector with one of "priors" or "param_value" specifying what to alter
#' @param x vector of real non negative values to be substituted into "priors" or "param_value"
#' @param alter_at string specifying filtering operations to be applied to parameters_df, yielding a logical vector indicating parameters for which values should be altered. (see examples)
#' @param node string indicating nodes which are to be altered
#' @param label string. Label for nodal type indicating nodal types for which values are to be altered. Equivalent to nodal_type.
#' @param nodal_type string. Label for nodal type indicating nodal types for which values are to be altered
#' @param param_set string indicating  the name of the set of parameters to be altered
#' @param given string indicates the node on which the parameter to be altered depends
#' @param statement causal query that determines nodal types for which values are to be altered
#' @param param_names string. The name of specific parameter in the form of, for example, 'X.1', 'Y.01'
#' @param distribution string indicating a common prior distribution (uniform, jeffreys or certainty)
#' @param normalize logical. If TRUE normalizes such that param set probabilities sum to 1.
#'
#' @keywords internal
#'
#' @examples
#'
#' # the below methods can be applied to either priors or param_values by specifying the desired option in \code{alter}
#'
#' model <- CausalQueries::make_model("X -> M -> Y; X <-> Y")
#'
#' #altering values using \code{alter_at}
#' CausalQueries:::make_par_values(model = model, x = c(0.5,0.25), alter_at = "node == 'Y' & nodal_type %in% c('00','01') & given == 'X.0'")
#'
#' #altering values using \code{param_names}
#' CausalQueries:::make_par_values(model = model, x = c(0.5,0.25), param_names = c("Y.10_X.0","Y.10_X.1"))
#'
#' #altering values using \code{statement}
#' CausalQueries:::make_par_values(model = model, x = c(0.5,0.25), statement = "Y[M=1] > Y[M=0]")
#'
#' #altering values using a combination of other arguments
#' CausalQueries:::make_par_values(model = model, x = c(0.5,0.25), node = "Y", nodal_type = c("00","01"), given = "X.0")


make_par_values <- function(model,
                            alter = "priors",
                            x = NA,
                            alter_at = NA,
                            node = NA,
                            label = NA,
                            nodal_type = NA,
                            param_set = NA,
                            given = NA,
                            statement = NA,
                            param_names = NA,
                            distribution = NA,
                            normalize = FALSE){

  full_param_set <- model$parameters_df$param_set
  full_node      <- model$parameters_df$node

  # check inputs for model, alter, x, distribution

  CausalQueries:::is_a_model(model)

  if(all(!is.na(c(nodal_type, label)))){
    stop("cannot define both nodal_type and label simultaniously; use nodal_type only")
  }

  if(!is.na(label)){
    warning("label is depreciated, use nodal_type instead")
    nodal_type <- label
  }


  if(!is.character(alter)){
    stop("alter must be of type character. No change to values.")
  }

  if(length(alter) != 1){
    stop("must provide 1 value to alter. No change to values.")
  }

  if(!(alter %in% c("priors","param_value"))){
    stop("type must be one of priors or param_value. No change to values.")
  }

  if(alter == "priors"){
    y <- model$parameters_df$priors
  } else {
    y <- model$parameters_df$param_value
  }

  if (all(is.na(distribution)) & all(is.na(x))){
    stop("neither distribution nor values provided. No change to values")
  }

  if (!all(is.na(x)) && !is.numeric(x)) {
    stop("arguments 'parameters' and 'alphas' must be a numeric vector. No change to values.")
  }

  if (!all(is.na(x)) && (min(x) < 0)){
    stop("arguments 'parameters' and 'alphas' must be non-negative.")
  }

  # disallow redundant argument specification

  if(!is.na(alter_at) & any(!is.na(list(node, nodal_type, param_set, given, statement, param_names)))){
    stop("Specifying alter_at with any of node, nodal_type, param_set, given, statement or param_names is redundant. No change to values.")
  }

  if(!is.na(param_names) & any(!is.na(list(node, nodal_type, param_set, given, statement)))){
    stop("Specifying param_names with any of node, nodal_type, param_set, given or statement is redundant. No change to values.")
  }

  if(all(!is.na(list(nodal_type, statement)))){
    stop("Specifying both nodal_type and statement is redundant. No change to values.")
  }

  if(all(!is.na(list(node, statement)))){
    stop("Specifying both node and statement is redundant. No change to values.")
  }

  if(!all(is.na(distribution)) & !all(is.na(x))){
    stop("values and distribution cannot be declared at the same time. Try sequentially. No change to values")
  }

  # construct command for alter_at

  if(!is.na(alter_at)){

    if(!is.character(alter_at)){
      stop("alter_at must be of type character")
    }

    cols <- unlist(strsplit(alter_at, "\\& | \\| | \\|\\| | \\&\\&"))%>%
      sapply(., function(i) strsplit(i, "\\== | \\%in% | \\!=")[[1]][1])%>%
      trimws(., which = "both")

    wrong_cols <- !(cols %in% c("parm_names","param_set","given","node","nodal_type"))

    if(any(wrong_cols)){
      stop(paste("In alter_at statement: specified columns;",
                 paste(cols[wrong_cols], collapse = ","),
                 "not in parameters_df. Please only specify column names present in parameters_df. This can be one of param_names, param_set, given, node or nodal_type. No change to values.",
                 sep = " "))
    }

    command <- paste("(",alter_at,")", sep = "")

  }

  # construct command for param_names

  if(!is.na(param_names)){

    if(!is.character(param_names)){
      stop("param_names must be of type character. No change to values.")
    }

    wrong_names <- !(param_names %in% model$parameters_df$param_names)

    if(any(wrong_names)){
      stop(paste("In param_names: specified parameters;",
           paste(param_names[wrong_names], collapse = ","),
           "not in model parameters. No change to values.",
           sep = " ")
           )
    }

    param_names <- param_names%>%
      paste(., collapse = "','")%>%
      paste("c('",.,"')", sep = "")

    command <- paste("(param_names %in% ", param_names, sep = "")

  }

  # construct command for remaining arguments

  if(any(!is.na(list(node, nodal_type, param_set, given, statement)))){

    args <- c("node","nodal_type","param_set","given","statement")
    defined <- args[!is.na(list(node, nodal_type, param_set, given, statement))]

    not_char <- sapply(eval(parse(text = paste("list(",paste(defined, collapse = ","),")"))),
                       function(i) !is.character(i))

    if(any(not_char)){
      stop(paste(paste(defined[not_char], sep = ","),
                 "must be of type character. No change to values.",
                 sep = " ")
           )
    }

    sub_mutate_at <- rep(NA, length(defined))

    #construct commands defining where to mutate for each argument
    for(j in 1:length(defined)){

      #if argument is statement get node and nodal type and construct command
      if(defined[j] == "statement"){
        query <- CausalQueries:::map_query_to_nodal_type(model, statement)

        node_j <- query$node%>%
          paste(., collapse = "','")%>%
          paste("c('",.,"')", sep = "")

        nodal_type_j <- names(which(query$types))%>%
          paste(., collapse = "','")%>%
          paste("c('",.,"')", sep = "")

        sub_mutate_at[j] <- paste("node %in% ", node_j, " & nodal_type %in% ", nodal_type_j, sep = "")

        #construct commands for other arguments
      } else {

        vec <- eval(parse(text = paste("unlist(", defined[j], ")", sep = "")))%>%
          paste(., collapse = "','")%>%
          paste("c('",.,"')", sep = "")

        sub_mutate_at[j] <- paste(defined[j], " %in% ", vec, sep = "")

      }

    }

    command <- paste("(", paste(sub_mutate_at, collapse = " & "), ")", sep = "")

  }

  if(!exists("command")){
    message("no specific parameters to alter values for specified. Altering all parameters.")
    to_alter <- rep(TRUE, length(y))
  } else {
    to_alter <- with(model$parameters_df, eval(parse(text = command)))
  }

  # Provide values unless a distribution is provided
  if(!is.na(distribution)){
    if(!(distribution %in% c("uniform", "jeffreys", "certainty"))){
      stop("distribution should be either 'uniform', 'jeffreys', or 'certainty'.")
    }
    x <- switch(distribution, uniform = 1, jeffreys = 0.5, certainty = 10000)
  }


  if(sum(to_alter) == 0){
    message("No change to values")
    out <- y

  } else {

    if ((length(x) != 1) & (length(x) != sum(to_alter))){
      stop(paste("Trying to replace ", sum(to_alter), " parameters with ", length(x), " values.",
                 "You either specified a wrong number of values or your conditions do not match the expected number of model parameters.",
                 sep = ""))
    }


    # Make changes: No normalization
    y[to_alter] <- x

    # When used for parameters, normalization is usually required
    # We normalize so that pars in a family set that has not been specified renormalize to
    # the residual of what has been specified
    if(normalize){

      sets_implicated <- unique(full_param_set[to_alter])

      for(j in sets_implicated){

        if(sum(y[to_alter & full_param_set==j])>1){
          warning("Provided values exceed 1 but normalization requested.")
        }

        y[!to_alter & full_param_set==j] <-
          y[!to_alter & full_param_set == j] * sum(1 - sum(y[to_alter & full_param_set == j])) / sum(y[!to_alter & full_param_set == j])

        y[full_param_set==j] <-
          y[full_param_set == j] / sum(y[full_param_set == j])

      }

    }

    out <- y

  }


  return(out)

}







