#' get_data_families
#'
#' Get possible data types
#'
#' @inheritParams CausalQueries_internal_inherit_params
#' @param drop_impossible Logical. Whether to drop data that is impossible given model restrictions. Defaults to `TRUE`.
#' @param drop_all_NA Logical. Whether to drop row of all `NA`s. Defaults to `TRUE`
#' @param mapping_only Logical. Whether to return data mapping matrix only. Defaults to `FALSE`.
#' @return Returns indices and ambiguity matrix
#' @keywords internal
#'
#' @importFrom dplyr filter
#' @examples
#' \donttest{
#' CausalQueries:::get_data_families(model = make_model('X->Y'))
#' CausalQueries:::get_data_families(model = make_model('X->Y'), mapping_only = TRUE)
#' CausalQueries:::get_data_families(model = make_model('X-> M -> Y'))
#'
#' }

get_data_families <- function(model, drop_impossible = TRUE, drop_all_NA = TRUE, mapping_only = FALSE) {

    event <- NULL

    # Get nodes
    nodes <- model$nodes

    # Get all possible data realizations, given strategies in which some data is not sought (NA).
    all_data <- all_data_types(model)

    # Get the realizations of the fundamental *possible* data events
    possible_data_types <- unique(data_type_names(model, realise_outcomes(model)))
    full_data <- filter(all_data, apply(all_data[, -1, drop = FALSE], 1, function(j) !any(is.na(j)))) %>% filter(event %in%
        possible_data_types)

    # Make E: Sign matrix used to see if data is *inconsistent* with reduced type
    sign_matrix <- (2 * as.matrix(all_data[nodes]) - 1)
    sign_matrix[is.na(sign_matrix)] <- 0

    type_matrix <- (2 * (as.matrix(full_data[nodes])) - 1)


    E <- 1 * matrix(apply(sign_matrix, 1, function(j) apply(type_matrix, 1, function(k) !(any(k * j == -1)))),
                    nrow = length(all_data$event),
                    byrow = TRUE)

    rownames(E) <- all_data$event
    colnames(E) <- full_data$event

    # Filtering
    keep <- rep(TRUE, nrow(E))
    if (drop_impossible)
        keep[!(apply(E, 1, function(j) any(j == 1)))] <- FALSE
    if (drop_all_NA)
        keep[rownames(E) == "None"] <- FALSE

   E <- E[keep,, drop = FALSE]


    all_data <- all_data[keep,]

    possible_events <- rownames(E)

    ## STRATEGIES ##############################

    # Figure out what strategy is being used in each of the possible data realizations
    which_strategy <- apply(all_data[nodes], 1, function(row) nodes[!is.na(row)])
    which_strategy <- which_strategy[lapply(which_strategy, length) != 0]

    if (!mapping_only) {
        E <- data.frame(event = possible_events, strategy = unlist(lapply(which_strategy, paste, collapse = "")),
            E, stringsAsFactors = FALSE)

        rownames(E) <- E$event
    }

    return(E)


}

#' Make compact data with data strategies
#'
#' Take a `data.frame` and return compact `data.frame`  of event types and strategies.
#'
#' @inheritParams CausalQueries_internal_inherit_params
#'
#' @param drop_NA Logical. Whether to exclude strategy families that contain no observed data. Exceptionally if no data is provided, minimal data on data on first node is returned. Defaults to `TRUE`
#' @param drop_family Logical. Whether to remove column \code{strategy} from the output. Defaults to `FALSE`.
#' @param summary Logical. Whether to return summary of the data. See details.  Defaults to `FALSE`.
#' @export
#'
#' @importFrom dplyr left_join
#' @importFrom dplyr filter mutate
#'
#' @return A vector of data events
#'
#' If \code{summary = TRUE} `collapse_data` returns a list containing the following components:
#' \item{data_events}{A compact data.frame of event types and strategies.}
#'    \item{observed_events}{A vector of character strings specifying the events observed in the data}
#'    \item{unobserved_events}{A vector of character strings specifying the events not observed in the data}
#' @examples
#'\donttest{
#'
#' model <- make_model('X -> Y')
#'
#' df <- data.frame(X = c(0,1,NA), Y = c(0,0,1))
#'
#' df %>% collapse_data(model)
#'
#'
#' collapse_data(df, model, drop_NA = FALSE)
#'
#' collapse_data(df, model, drop_family = TRUE)
#'
#' collapse_data(df, model, summary = TRUE)
#'
#' data <- make_data(model, n = 0)
#' collapse_data(data, model)
#'
#' model <- make_model('X -> Y') %>% set_restrictions('X[]==1')
#' df <- simulate_data(model, n = 10)
#' df[1,1] <- ''
#' collapse_data(df, model)
#' data <- data.frame(X= 0:1)
#' collapse_data(data, model)
#'
#' }
#'


collapse_data <- function(data, model, drop_NA = TRUE, drop_family = FALSE, summary = FALSE) {

    # Add missing nodes and order correctly
    nodes <- model$nodes
    if (any(!(nodes %in% names(data)))){
      data[nodes[!(nodes %in% names(data))]] <- NA
    }

    data <- data[, nodes, drop = FALSE]

    if (nrow(data) == 0 | all(is.na(data))) {
        data_events <- minimal_event_data(model)
        drop_NA <- FALSE

    } else {

        data_families <- get_data_families(model)[, c("event", "strategy")]
        data_type <- data_type_names(model, data)

        # Inconsistent data
        if (!all(unique(data_type) %in% data_families$event))
            message(paste0(unique(data_type)[!(unique(data_type) %in% data_families$event)], " data is inconsistent with model and ignored"))

        # Collapse
        data_events <- data.frame(table(data_type), stringsAsFactors = FALSE)
        colnames(data_events) <- c("event", "count")
        data_events$event <- as.character(data_events$event)

        # Merge in families
        data_events <- left_join(data_families, data_events, by = "event") %>% mutate(count = ifelse(is.na(count),
            0, count))

    }

    # Output varies according to args
    if (drop_NA) {
        data_events <- drop_empty_families(data_events)
    }
    if (drop_family) {
        data_events <- dplyr::select(data_events, -"strategy")
    }
    if (summary) {
        return(list(data_events = data_events, observed_events = with(data_events, unique(event[count >
            0])), unobserved_events = with(data_events, unique(event[count == 0]))))
    } else {
        return(data_events)
    }

}


#' Drop empty families
#'
#' @inheritParams CausalQueries_internal_inherit_params
#'
#' @return Returns data events with strategies (excluding  strategy families that contain no observed data)
#' @keywords internal
#' @examples
#'\donttest{
#' data_events <- data.frame(event = c('X0Y0', 'Y0'), strategy = c('XY', 'Y'), count = 1:0)
#' CausalQueries:::drop_empty_families(data_events)
#'  }
#'
drop_empty_families <- function(data_events) {

    for (j in unique(data_events$strategy)) {
        if (sum(data_events$count[data_events$strategy == j]) == 0) {
            data_events <- dplyr::filter(data_events, strategy != j)
        }
    }
    return(data_events)

}


#' Expand compact data object to data frame
#'
#' @inheritParams CausalQueries_internal_inherit_params
#' @return A \code{data.frame} with rows as data observation
#' @export
#' @examples
#' \donttest{
#' model <- make_model('X->M->Y')
#' make_events(model, n = 5) %>%
#'   expand_data(model)
#' make_events(model, n = 0) %>%
#'   expand_data(model)
#'  }
#'
expand_data <- function(data_events = NULL, model) {

    if (!is(model, "causal_model"))
        stop("model should be a model generated with make_model")
    if (is.null(data_events))
        data_events <- minimal_event_data(model)
    if ((!is.data.frame(data_events) & !is.matrix(data_events)) |
          any(!c("event", "count") %in% colnames(data_events)))
       stop( "data_events should be a data frame or a matrix with columns `event` and `count`")
    if ("strategy" %in% names(data_events))
        data_events <- dplyr::select(as.data.frame(data_events), c("event", "count"))

    if (sum(data_events[, 2]) == 0)
        return(minimal_data(model))  # Special case with no data

    vars <- model$nodes
    df <- merge(all_data_types(model), data_events, by.x = "event")
    xx <- unlist(sapply(1:nrow(df), function(i) replicate(df[i, ncol(df)], df[i, vars])))
    out <- data.frame(matrix(xx, ncol = length(vars), byrow = TRUE))
    names(out) <- vars
    return(out)
}


#' Data type names
#'
#' Provides names to data types
#' @inheritParams CausalQueries_internal_inherit_params
#' @return A vector of strings of data types
#' @examples
#' model <- make_model('X -> Y')
#' data <- simulate_data(model, n = 2)
#' data_type_names(model, data)
#' @export
data_type_names <- function(model, data) {
    vars <- model$nodes
    data <- data[vars]
    data[data == ""] <- NA
    out <- apply(data, 1, function(j) paste(paste0(vars[!is.na(j)], j[!is.na(j)]), collapse = ""))
    out[out == ""] <- "None"
    return(out)
}

#' All data types
#'
#' Creates dataframe with all data types (including NA types) that are possible from a model.
#'
#' @inheritParams CausalQueries_internal_inherit_params
#' @param complete_data Logical. If `TRUE` returns only complete data types (no NAs). Defaults to `FALSE`.
#' @param possible_data Logical. If `TRUE` returns only complete data types (no NAs) that are *possible* given model restrictions. Note that in principle an intervention could make observationally impossible data types arise. Defaults to `FALSE`.
#' @param given A character.  A quoted statement that evaluates to logical. Data conditional on specific values.
#' @return A \code{data.frame} with all data types (including NA types) that are possible from a model.
#' @export
#' @examples
#' \donttest{
#' all_data_types(make_model('X -> Y'))
#' model <- make_model('X -> Y') %>% set_restrictions(labels = list(Y = '00'), keep = TRUE)
#'   all_data_types(model)
#'   all_data_types(model, complete_data = TRUE)
#'   all_data_types(model, possible_data = TRUE)
#'   all_data_types(model, given  = 'X==1')
#'   all_data_types(model, given  = 'X==1 & Y==1')
#'}
all_data_types <- function(model, complete_data = FALSE, possible_data = FALSE, given = NULL) {
    nodes <- model$nodes
    # If complete_data allow only 2 possible realizations (0,1), otherwise 3 (0,1,NA)
    r <- ifelse(complete_data, 1, 2)
    m <- length(model$nodes)
    df <- data.frame(perm(rep(r, m))) - 1 + complete_data
    df[df == -1] <- NA
    names(df) <- nodes
    df <- data.frame(cbind(event = data_type_names(model, df), df))

    order_list <- c(list(rowSums(is.na(df))), lapply(rev(nodes), function(node) is.na(df[, node])),
        lapply(rev(nodes), function(node) df[, node]))

    df <- df[do.call(what = order, args = order_list), ]

    if (possible_data) {
        possible_data_types <- unique(data_type_names(model, realise_outcomes(model)))
        df <- dplyr::filter(df, event %in% possible_data_types)
    }

    # exclude data not consistent with 'given' (NAs are *not* consistent with given)
    if (!is.null(given)) {
        take <- with(df, eval(parse(text = given)))
        take[is.na(take)] <- FALSE
        df <- df[take, ]
    }
    rownames(df) <- df$event

    return(df)
}

#' Creates a compact data frame for case with no data
#' @param model A \code{causal_model}. A model object generated by \code{\link{make_model}}.
#' @return A compact data frame where each row represents an element from the exhaustive set of events of a model. The count for each event is set to zero.
#' @keywords internal
#' @examples
#' \donttest{
#' model <- make_model('X -> K -> Y')
#' CausalQueries:::minimal_event_data(model)
#' }

minimal_event_data <- function(model){
  simulate_data(model, n = 1) %>%
    collapse_data(model) %>%
    mutate(count = 0)
}


#' Creates a data frame for case with no data
#' @param model A \code{causal_model}. A model object generated by \code{\link{make_model}}.
#' @return A \code{data.frame} with one row of NAs and columns named according to nodes in a model.
#' @keywords internal
#' @examples
#' \donttest{
#' model <- make_model('X -> K -> Y')
#' CausalQueries:::minimal_data(model)
#' }
minimal_data <- function(model) {
    vars <- model$nodes
    df <- data.frame(t(rep(NA, length(vars))))
    names(df) <- vars
    return(df)
}

