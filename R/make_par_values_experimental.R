make_par_values_exper <- function(model,
                                  x = NA,
                                  node = NA,
                                  nodal_type = NA,
                                  param_set = NA,
                                  statement = NA,
                                  param_names = NA,
                                  distribution = NA,
                                  normalize = FALSE){

  params <- model$parameters_df

  #check argument length
  arg_length <- sapply(list(x,node,nodal_type,statement,param_names,distribution), length)

  if (sum(arg_length > 1) > 1){
    if (sd(arg_length[arg_length > 1]) > 0){
      stop("Provided arguments of length >1 should be of the same length")
    }
  }

  #loop over argument sets and construct commands defining where to mutate
  args <- c("node","nodal_type","param_set","statement","param_names")

  mutate_at <- rep(NA,arg_length[1])

  for(i in 1:length(mutate_at)){

    #disallow redundant argument definition
    if(!is.na(param_names[i]) & any(!is.na(c(node[i], nodal_type[i], param_set[i], statement[i])))){
      stop(paste("In argument position ", i, ": specifying param_names with any of node, nodal_type, param_set or statement is redundant. No change to values.", sep = ""))
    }

    if(all(!is.na(c(nodal_type[i], statement[i])))){
      stop(paste("In argument position ", i, ": specifying both nodal_type and statement is redundant. No change to values.", sep = ""))
    }

    if(all(!is.na(c(node[i], statement[i])))){
      stop(paste("In argument position ", i, ": specifying both node and statement is redundant. No change to values.", sep = ""))
    }

    #figure out which arguments are defined
    defined <- sapply(list(node,nodal_type,param_set,statement,param_names), function(j) j[i])
    defined <- args[which(!is.na(defined))]

    #if param names is defined construct mutate statement
    if("param_names" %in% defined){

      mutate_at[i] <- paste("dplyr::mutate(priors = replace(priors, which(param_names == '", param_names[i], "'), ", unlist(x[i]), "))", sep = "")

    #if other set of arguments is defined figure out where to mutate
    } else {

      sub_mutate_at <- rep(NA, length(defined))

      #construct commands defining where to mutate for each argument
      for(j in 1:length(defined)){

        #if argument is statement get node and nodal type and construct command
        if(defined[j] == "statement"){
          query <- CausalQueries:::map_query_to_nodal_type(model, statement[i])

          node_j <- query$node%>%
            paste(., collapse = "','")%>%
            paste("c('",.,"')", sep = "")

          nodal_type_j <- names(which(query$types))%>%
            paste(., collapse = "','")%>%
            paste("c('",.,"')", sep = "")

          sub_mutate_at[j] <- paste("node %in% ", node_j, " & nodal_type %in% ", nodal_type_j, sep = "")

        #construct commands for other arguments
        } else {

          vec <- eval(parse(text = paste("unlist(", defined[j], "[i])", sep = "")))%>%
            paste(., collapse = "','")%>%
            paste("c('",.,"')", sep = "")

          sub_mutate_at[j] <- paste(defined[j], " %in% ", vec, sep = "")

        }

      }

      mutate_at[i] <- paste("dplyr::mutate(priors = replace(priors, which(", paste(sub_mutate_at, collapse = " & "), "), ", unlist(x[i]), "))", sep = "")

    }

  }

  params <- paste("model$parameters_df", paste(mutate_at, collapse = "%>%"), sep = "%>%")%>%
    parse(text = .)%>%
    eval(envir = parent.frame())

  return(params)

}





