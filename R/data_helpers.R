#' Make compact data with data strategies
#'
#' Take a dataframe and return compact dataframe of event types and strategies.
#'
#' @param data A data.frame of variables that can take three values: 0, 1, and NA
#' @param model A model created by make_model()
#'
#' @export
#'
#' @return A vector of data events
get_data_events <- function(data, model){

	likelihood_helpers  <- get_likelihood_helpers(model)
	possible_events     <- likelihood_helpers$possible_events
	possible_strategies <- names(likelihood_helpers$w_starts)
	variables           <- model$variables
	i_strategy          <- likelihood_helpers$w_ends - likelihood_helpers$w_starts +1


	if(!all(variables %in% names(data))){stop("Could not find all of the variables in the DAG in
																						the data you provided.\nPlease double-check variable
																						names and try again.")}

	revealed_data <- reveal_outcomes(model)
	data <- data[,variables]
  data[data == ""] <- NA
	# Check if observed data is in conflict with restrictions
	inconsistencies <- !apply(data, 1, function(observed){
		any(apply(revealed_data, 1, function(possible){
			!any(possible[!is.na(observed)] != observed[!is.na(observed)])}
		))
	})

	if(any(inconsistencies)){
		message(paste("Observations are not consistent with restrictions in", sum(inconsistencies), "cases"))
		data <- data[!inconsistencies, ]
	}

	# replace "" with na and remove rows where all values are na
	data <- data[!apply(data, 1,  function(x) all(is.na(x) | x == "")),]
	data[is.na(data)] <- ""


	# Data events dataframe
	data_type <- apply(X = data, MARGIN = 1, FUN = function(row){
		paste0(variables[!(row == "")],row[!(row == "")], collapse = "")})

	data_events <- data.frame(event = possible_events,
														strategy   = rep(possible_strategies, i_strategy),
														count = 0,
														stringsAsFactors = FALSE,
														row.names = NULL)

	data_events$count <- sapply(data_events$event, function(j) sum(data_type == paste(j)))

	# Output
	list(
		data_events       = data_events,
		observed_events   = with(data_events, unique(event[count>0])),
		unobserved_events = with(data_events, unique(event[count==0])),
		used_strategies   = with(data_events, unique(strategy[count>0])),
		unused_strategies = with(data_events, unique(strategy[count==0]))
	)

}


#' Summarize Data
#'
#' @param model A probabilistic causal model created by  \code{make_model()}
#' @param data A data.frame of variables that can take three values: 0, 1, and NA.
#' @return Returns data events wtih strategies (excluding  strategy families that contain no observed data)
#'
#' @export
#' @examples
#' model <- make_model("X -> Y")
#' data <- simulate_data(model, n = 10)
#' data[1,1] <- ""
#' summarize_data(model, data)
#'
summarize_data <- function(model, data){
	if(!all(model$variables %in% names(data))) stop("All model variables should be in data provided (even if these take value NA only)")

	# 1. Get data and delete all rows from strategies that contained no observed data
	data_events <- get_data_events(data = data, model = model)$data_events
	if(all(is.na(data))) return(data_events)
	data_events_split <- split(data_events, as.factor(data_events$strategy))
	data_events_w_NA <- do.call(rbind,lapply(data_events_split, function(df){
		out <- df
		if(sum(df$count) == 0){
			out[,] <- NA
		}
		out
	}))

	delete_strategies <- !apply(data_events_w_NA, 1, function(x) all(is.na(x)))
	r_names <- rownames(data_events)
	out <- data_events_w_NA[delete_strategies, ]
	rownames(out) <- 1:nrow(out)
	out
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
#' data <- simulate_data(model, n = 10)
#' data[1,1] <- ""
#' data[3,2] <- NA
#'
#' encode_data(model, data)
#' table(encode_data(model, data))
encode_data <- function(model, data){
	data[is.na(data)] <- ""
	data[data ==""] <- NA
	vars <- model$variables
	apply(data, MARGIN = 1, FUN = function(row){
		paste0(vars[!(is.na(row))],row[!(is.na(row))], collapse = "")})
}


#' Make data compact with data as first argument
#'
#' @param data A data.frame.
#' @param model A model
#' @param remove_family Logical. If `FALSE`, removes column "family" from the output.
#' @export
#' @examples
#' model <- make_model("X -> Y")
#' data <- simulate_data(model, n = 10)
#' collapse_data(data, model)
#'
collapse_data <- function(data, model, remove_family = TRUE){
	x <- summarize_data(model = model, data)
	if(remove_family) x <- x[, c("event", "count")]
	x
}

#' Expand compact data object to data frame
#'
#' @param data_events A compact data frame compatible with \code{model}.
#' @param model A model
#' @export
#' @examples
#' model <- make_model("X->M->Y")
#' draw_data_events(model, n = 5) %>%
#'   expand_data(model)

expand_data <- function(data_events, model) {

	if(class(model) != "causal_model") stop("model should be a model generated with make_model")
	if(!is.data.frame(data_events)) stop("data_events should be a data frame with columns `event` and `count`")

	vars <- model$variables
	df   <- merge(all_data_types(model), data_events, by.x = "event")
	xx   <- unlist(sapply(1:nrow(df), function(i) replicate(df[i,ncol(df)], df[i, vars])))
	out  <- data.frame(matrix(xx, ncol = length(vars), byrow = TRUE))
	names(out) <- vars
	out
}


#' Data type names
#'
#' Provides names to data types
#' @param model A model object generated by \code{make_model()}.
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
#' @param model A model object generated by \code{make_model()}.
#' @export
#' @examples
#' all_data_types(make_model("X -> Y"))
all_data_types <- function(model) {
	variables <- model$variables
	m <- length(model$variables)
	df <- data.frame(perm(rep(2, m))) - 1
	df[df==-1] <- NA
	names(df) <-  variables
	data.frame(cbind(event = data_type_names(model, df), df))
}
