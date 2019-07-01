#' Data type names
#'
#' Provides names to data types
#' @param model A model created by make_model()
#' @param data Data in long form
#' @export
#' @examples
#' model <- make_model("X -> Y")
#' data <- simulate_data(model, n = 2)
#' data_type_names(model, data)
#'
data_type_names <- function(model, data){
	vars <- model$variables
	data <- data[vars]
	data[data==""] <- NA
	out <- apply(data, 1, function(j) paste(paste0(vars[!is.na(j)], j[!is.na(j)]), collapse = ""))
	out[out == ""] <- "None"
	out}

#' All data types
#'
#' Creates dataframe with all data types, including NA types possible from a model
#'
#' @param model A model created by make_model()
#' @export
#' @examples
#' all_data_types(make_model("X -> Y"))

all_data_types <- function(model) {
	variables <- model$variables
	m <- length(model$variables)
	df <- data.frame(gbiqq::perm(rep(2, m))) - 1
	df[df==-1] <- NA
	names(df) <-  variables
	data.frame(cbind(event = data_type_names(model, df), df))
}


#' Encode data
#'
#' Takes data in long format, including NA values or blanks and returns vector with each row encoded as a data type.
#'
#' @param model A  model
#' @param data Data in long format
#' @export
#' @examples
#' model <- make_model("X -> Y")
#' data <- simulate_data(model, n = 4)
#' data[1,1] <- ""
#' data[3,2] <- NA
#'
#' encode_data(model, data)
encode_data <- function(model, data){
	data[data ==""] <- NA
	vars <- model$variables
	apply(data, MARGIN = 1, FUN = function(row){
		paste0(vars[!(is.na(row))],row[!(is.na(row))], collapse = "")})
}