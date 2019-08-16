

#' Summarize Data
#'
#' @param model A probabilistic causal model created by  \code{make_model()}
#' @param data A data.frame of variables that can take three values: 0, 1, and NA.
#' @return Returns data events without strategies containing no observed data
#'
#' @export
summarize_data <- function(model, data){
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
	trimmed_data_events <- data_events_w_NA[delete_strategies, ]
	rownames(trimmed_data_events) <- 1:nrow(trimmed_data_events)
	trimmed_data_events

}



#' Summarize data events
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


	# Stop if observed data is not in conflict with restrictions
	inconsistencies <- !apply(data, 1, function(observed){
		any(apply(revealed_data, 1, function(possible){
			!any(possible[!is.na(observed)] != observed[!is.na(observed)])}
		))
	})

	if(any(inconsistencies)){
		message(paste("Observations are not consistent with restrictions in", sum(!inconsistencies), "cases"))
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


