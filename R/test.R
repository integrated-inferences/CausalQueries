## list of filters -------------------------------------------------------------

# Define the vectors of values
node_values <- c('Y')
nodal_type_values <- c('01', '00')
given_values <- c('X.0')

# Create a data frame with all combinations of values
combinations <- expand.grid(node = node_values, nodal_type = nodal_type_values, given = given_values)

# Generate separate filter conditions for each combination
filter_conditions <- lapply(1:nrow(combinations), function(i) {
  node_filter <- paste0("node %in% c('", combinations[i, "node"], "')")
  nodal_type_filter <- paste0("nodal_type %in% c('", combinations[i, "nodal_type"], "')")
  given_filter <- paste0("given %in% c('", combinations[i, "given"], "')")

  filter_condition <- paste(node_filter, nodal_type_filter, given_filter, collapse = " & ")
  return(filter_condition)
})

# Print the separate filter conditions
print(filter_conditions)


## alter_at tests --------------------------------------------------------------

rm(list = ls())
alter_at <- "node == 'Y' & nodal_type %in% c('00','01') & given == 'X.0'"

cols <- unlist(strsplit(alter_at, "\\& | \\| | \\|\\| | \\&\\&"))%>%
  sapply(function(i) strsplit(i, "\\== | \\%in% | \\!=")[[1]][1])%>%
  trimws(which = "both")

join <- stringr::str_extract_all(alter_at, "\\& | \\| | \\|\\| | \\&\\&")[[1]]
operation <- unlist(strsplit(alter_at, "\\& | \\| | \\|\\| | \\&\\&"))
cols <- sapply




d1 <- data.frame(V1 = c(1,2,3,4),
                 V2 = c(10,20,30,40))



  sapply(function(i) strsplit(i, "\\== | \\%in% | \\!=")[[1]])





