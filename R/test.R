## alter_at tests --------------------------------------------------------------

rm(list = ls())
alter_at <- "node == 'Y' & nodal_type %in% c('00','01') & given == 'X.0'"


join <- stringr::str_extract_all(alter_at, "\\& | \\| | \\|\\| | \\&\\&")[[1]] |>
  trimws(which = "both") |>
  stringr::str_pad(width = 4, side = "both")


operation <- unlist(strsplit(alter_at, "\\& | \\| | \\|\\| | \\&\\&"))

operand <- sapply(operation, function(i) stringr::str_extract_all(i, "\\%in% | \\== | \\!=")[[1]]) |>
  trimws(which = "both")

sets <- mapply(function(operation,operand) {
  split <- strsplit(operation, operand)[[1]]
  list(paste(paste("param_df", split[1], sep = "$"), operand, sep = " "),
       eval(parse(text = split[2])))
}, operation, operand, SIMPLIFY = FALSE)

commands <- lapply(sets, function(i) i[[2]]) |>
  expand.grid(stringsAsFactors = FALSE)

for(i in 1:length(sets)) {
  commands[[i]] <- paste(sets[[i]][[1]],paste("'",commands[[i]],"'", sep = ""), sep = " ")
}

commands <- apply(commands, 1, function(row) paste(row, collapse = join))

## param_names tests -----------------------------------------------------------
rm(list = ls())

param_names <- c("Y.10_X.0","Y.10_X.1")

commands <- paste("param_df$param_names == ", "'", param_names, "'", sep = "")


## remaining arguments tests ---------------------------------------------------

# statement
rm(list = ls())
model <- CausalQueries::make_model("X -> M -> Y; X <-> Y")
statement <- "Y[M=1] > Y[M=0]"


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

  sets <- as.list(rep(NA,4))
  names <- c("node","nodal_type","param_set","given")
  names(sets) <- names

  #construct commands defining where to mutate for each argument
  for(i in 1:length(defined)){
    #if argument is statement get node and nodal type and construct command
    if(defined[i] == "statement"){
      query <- map_query_to_nodal_type(model, statement, join_by = join_by)
      sets[["node"]] <- query$node
      sets[["nodal_type"]]<- names(which(query$types))
    } else {
      sets[[defined[i]]] <- eval(parse(text = paste("unlist(", defined[i], ")", sep = "")))
    }
  }

  commands <- expand.grid(sets,stringsAsFactors = FALSE)
  for(i in 1:length(sets)) {
    commands[[i]] <- paste(paste("param_df$", names[i], sep = ""), "==", paste("'",commands[[i]],"'", sep = ""), sep = " ")
  }

  commands <- commands[,defined]
  commands <- apply(commands, 1, function(row) paste(row, collapse = " & "))

}




