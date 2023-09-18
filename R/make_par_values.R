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
#' @param join_by string specifying the logical operator joining expanded types when \code{statement} contains wildcards. Can take values \code{'&'} (logical AND) or \code{'|'} (logical OR).
#' @param param_names string. The name of specific parameter in the form of, for example, 'X.1', 'Y.01'
#' @param distribution string indicating a common prior distribution (uniform, jeffreys or certainty)
#' @param normalize logical. If TRUE normalizes such that param set probabilities sum to 1.
#'
#' @keywords internal
#'
#' @examples
#'
#' # the below methods can be applied to either priors or
#' # param_values by specifying the desired option in \code{alter}
#'
#' model <- CausalQueries::make_model("X -> M -> Y; X <-> Y")
#'
#' #altering values using \code{alter_at}
#' CausalQueries:::make_par_values(model = model,
#' x = c(0.5,0.25),
#' alter_at = "node == 'Y' & nodal_type %in% c('00','01') & given == 'X.0'")
#'
#' #altering values using \code{param_names}
#' CausalQueries:::make_par_values(model = model,
#' x = c(0.5,0.25),
#' param_names = c("Y.10_X.0","Y.10_X.1"))
#'
#' #altering values using \code{statement}
#' CausalQueries:::make_par_values(model = model, x = c(0.5,0.25), statement = "Y[M=1] > Y[M=0]")
#'
#' #altering values using a combination of other arguments
#' CausalQueries:::make_par_values(model = model,
#' x = c(0.5,0.25), node = "Y", nodal_type = c("00","01"), given = "X.0")


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
                            join_by = "|",
                            param_names = NA,
                            distribution = NA,
                            normalize = FALSE){
  is_a_model(model)

  full_param_set <- model$parameters_df$param_set
  full_node <- model$parameters_df$node

  # check inputs for model, alter, x, distribution
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

  if(!is.character(join_by)){
    stop("join_by must be of type character")
  }

  if(!(join_by %in% c("|","&"))){
    stop("join_by must be one of '|' or '&'")
  }

  if(length(join_by) > 1){
    stop("please only define one join_by")
  }

  # disallow redundant argument specification
  if(!is.na(alter_at) & any(!is.na(list(node, nodal_type, param_set, given, statement, param_names)))){
    stop("Specifying alter_at with any of node, nodal_type, param_set, given, statement or param_names is redundant. No change to values.")
  }

  if(any(!is.na(param_names)) & any(!is.na(list(node, nodal_type, param_set, given, statement)))){
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

  #disallow multiple statements
  if(!is.na(statement) & length(statement) > 1) {
    stop("please specify only one statement")
  }

  # construct commands for alter_at
  if(!is.na(alter_at)){

    if(!is.character(alter_at)){
      stop("alter_at must be of type character")
    }

    # extract logical operations that join filter statements
    join <- stringr::str_extract_all(alter_at, "\\& | \\| | \\|\\| | \\&\\&")[[1]] |>
      trimws(which = "both") |>
      stringr::str_pad(width = 4, side = "both")

    # extract filter statements
    operation <- unlist(strsplit(alter_at, trimws(join, which = "both")))

    # extract operands from filter statements
    operand <- sapply(operation, function(i) stringr::str_extract_all(i, "\\%in% | \\== | \\!=")[[1]]) |>
      trimws(which = "both")

    # check if correct columns are specified in filter statements
    cols <- mapply(function(operation, operand) {
      sub(paste(operand, ".*", sep = ""), "", operation) |>
        trimws(which = "both")
    }, operation, operand)

    wrong_cols <- !(cols %in% c("parm_names","param_set","given","node","nodal_type"))

    if(any(wrong_cols)){
      stop(paste("In alter_at statement: specified columns;",
                 paste(cols[wrong_cols], collapse = ","),
                 "not in parameters_df. Please only specify column names present in parameters_df. This can be one of param_names, param_set, given, node or nodal_type. No change to values.",
                 sep = " "))
    }

    # split filter statements into column + operand and filter values
    sets <- mapply(function(operation,operand) {
      split <- strsplit(operation, operand)[[1]]
      list(paste(paste("param_df", split[1], sep = "$"), operand, sep = " "),
           eval(parse(text = split[2])))
    }, operation, operand, SIMPLIFY = FALSE)

    # call expand.grid on filter values >> this allows for the construction of a unique filter
    # command for each parameter
    commands <- lapply(sets, function(i) i[[2]]) |>
      expand.grid(stringsAsFactors = FALSE)

    # construct filter commands
    for(i in 1:length(sets)) {
      commands[[i]] <- paste(sets[[i]][[1]],paste("'",commands[[i]],"'", sep = ""), sep = " ")
    }

    commands <- apply(commands, 1, function(row) paste(row, collapse = join))
  }

  # construct commands for param_names
  if(any(!is.na(param_names))){

    if(!is.character(param_names)){
      stop("param_names must be of type character. No change to values.")
    }

    # check if correct param_names specified
    wrong_names <- !(param_names %in% model$parameters_df$param_names)

    if(any(wrong_names)){
      stop(paste("In param_names: specified parameters;",
           paste(param_names[wrong_names], collapse = ","),
           "not in model parameters. No change to values.",
           sep = " ")
           )
    }

    # construct commands for param_names
    commands <- paste("param_df$param_names == ", "'", param_names, "'", sep = "")
  }

  # construct command for remaining arguments
  if(any(!is.na(list(node, nodal_type, param_set, given, statement)))){

    # check which arguments are given
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

    sets <- as.list(rep(NA,4))
    names <- c("node","nodal_type","param_set","given")
    names(sets) <- names

    # construct commands defining where to filter for each argument
    for(i in 1:length(defined)){
      #if argument is statement place filter values in node and nodal_type position for argument list
      if(defined[i] == "statement"){
        query <- map_query_to_nodal_type(model, statement, join_by = join_by)
        sets[["node"]] <- query$node
        sets[["nodal_type"]]<- names(which(query$types))
      } else {
        #place filter values in argument list
        sets[[defined[i]]] <- eval(parse(text = paste("unlist(", defined[i], ")", sep = "")))
      }
    }

    # expand.grid on argument list >> this allows for the construction of a unique filter
    # command for each parameter
    commands <- expand.grid(sets,stringsAsFactors = FALSE)

    # construct commands
    for(i in 1:length(sets)) {
      commands[[i]] <- paste(paste("param_df$", names[i], sep = ""), "==", paste("'",commands[[i]],"'", sep = ""), sep = " ")
    }

    if("statement" %in% defined) {
      defined <- defined[!defined == "statement"]
      defined <- c(defined, "node", "nodal_type")
    }
                
    commands <- commands[,defined, drop = FALSE]
    commands <- apply(commands, 1, function(row) paste(row, collapse = " & "))

  }


  # Provide values unless a distribution is provided
  if(!is.na(distribution)){
    if(!(distribution %in% c("uniform", "jeffreys", "certainty"))){
      stop("distribution should be either 'uniform', 'jeffreys', or 'certainty'.")
    }
    x <- switch(distribution, uniform = 1, jeffreys = 0.5, certainty = 10000)
  }

  # check if correct number of values specified
  if(!exists("commands")) {
    n_to_alter <- length(y)
  } else {
    n_to_alter <- length(commands)
  }

  if((n_to_alter > 0) & (length(x) != 1) & (length(x) != n_to_alter)) {
    stop(paste("Trying to replace ", n_to_alter, " parameters with ", length(x), " values.",
               "You either specified a wrong number of values or your conditions do not match the expected number of model parameters.",
               sep = ""))
  }


  # generate where to alter
  if(!exists("commands")){
    if(!is.na(distribution)){
      message("no specific parameters to alter values for specified. Altering all parameters.")
    }
    to_alter <- rep(TRUE, length(y))
  } else {
    # reorder new parameter values according to the parameter order in parameters_df
    param_df <- model$parameters_df
    names <- sapply(commands, function(i) {
      eval(parse(text = paste("param_df[", i, ",][['param_names']]")))
    })

    names(x) <- names
    x <- x[param_df$param_names]
    to_alter <- !is.na(x)
    x <- x[to_alter]
  }

  # apply changes
  if(sum(to_alter) == 0){
    message("No change to values")
    out <- y
  } else {

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








