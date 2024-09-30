#' Data helpers
#'
#' Various helpers to simulate data and to manipulate data types between compact and long forms.
#'
#' @name data_helpers
NULL
#> NULL



#' Make compact data with data strategies
#'
#' Take a `data.frame` and return compact `data.frame`
#' of event types and strategies.
#'
#' @rdname data_helpers
#' @inheritParams CausalQueries_internal_inherit_params
#'
#' @param drop_NA Logical. Whether to exclude strategy families that contain
#'   no observed data. Exceptionally if no data is provided, minimal data on
#'   data on first node is returned. Defaults to `TRUE`
#' @param drop_family Logical. Whether to remove column \code{strategy} from
#'   the output. Defaults to `FALSE`.
#' @param summary Logical. Whether to return summary of the data. See details.
#'   Defaults to `FALSE`.
#' @export
#'
#' @importFrom dplyr left_join
#' @importFrom dplyr filter mutate
#'
#' @return A vector of data events
#'
#' If \code{summary = TRUE} `collapse_data` returns a list containing the
#'   following components:
#' \item{data_events}{A compact data.frame of event types and strategies.}
#'    \item{observed_events}{A vector of character strings specifying the events
#'      observed in the data}
#'    \item{unobserved_events}{A vector of character strings specifying the
#'      events not observed in the data}
#' @examples
#'\donttest{
#'
#' model <- make_model('X -> Y')
#'
#' df <- data.frame(X = c(0,1,NA), Y = c(0,0,1))
#'
#' df |> collapse_data(model)
#'
#' # Illustrating options
#'
#' df |> collapse_data(model, drop_NA = FALSE)
#'
#' df |> collapse_data(model, drop_family = TRUE)
#'
#' df |> collapse_data(model, summary = TRUE)
#'
#' # Appropriate behavior given restricted models
#'
#' model <- make_model('X -> Y') |>
#'   set_restrictions('X[]==1')
#' df <- make_data(model, n = 10)
#' df[1,1] <- ''
#' df |> collapse_data(model)
#'
#' df <- data.frame(X = 0:1)
#' df |> collapse_data(model)
#'
#' }
#'


collapse_data <- function(data,
                          model,
                          drop_NA = TRUE,
                          drop_family = FALSE,
                          summary = FALSE) {

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
          message(paste0(
            unique(data_type)[!(unique(data_type) %in% data_families$event)],
            " data is inconsistent with model and ignored")
            )

        # Collapse
        data_events <- data.frame(table(data_type), stringsAsFactors = FALSE)
        colnames(data_events) <- c("event", "count")
        data_events$event <- as.character(data_events$event)

        # Merge in families
        data_events <- left_join(data_families, data_events, by = "event") |>
          mutate(count = ifelse(is.na(count), 0, count))

    }

    # Output varies according to args
    if (drop_NA) {
        data_events <- drop_empty_families(data_events)
    }
    if (drop_family) {
        data_events <- dplyr::select(data_events, -"strategy")
    }
    if (summary) {
      return(list(
        data_events = data_events,
        observed_events = with(data_events, unique(event[count > 0])),
        unobserved_events = with(data_events, unique(event[count == 0]))
      ))
    } else {
      return(data_events)
    }

}



#' Expand compact data object to data frame
#'
#' @rdname data_helpers
#' @inheritParams CausalQueries_internal_inherit_params
#' @return A \code{data.frame} with rows as data observation
#' @export
#' @examples
#' \donttest{
#' model <- make_model('X->M->Y')
#' make_events(model, n = 5) |>
#'   expand_data(model)
#' make_events(model, n = 0) |>
#'   expand_data(model)
#'  }
#'
expand_data <- function(data_events = NULL, model) {

  if (!is(model, "causal_model")) {
    stop("model should be a model generated with make_model")
  }

  if (is.null(data_events)) {
    data_events <- minimal_event_data(model)
  }

  if ((!is.data.frame(data_events) & !is.matrix(data_events)) |
      any(!c("event", "count") %in% colnames(data_events))) {
    stop(paste("data_events should be a data frame or a",
               "matrix with columns `event` and `count`"))
  }

  if ("strategy" %in% names(data_events)) {
    data_events <-
      dplyr::select(as.data.frame(data_events), c("event", "count"))
  }

  if (sum(data_events[, 2]) == 0) {
    return(minimal_data(model))  # Special case with no data
  }


  vars <- model$nodes
  df <- merge(get_all_data_types(model), data_events, by.x = "event")
  xx <- unlist(lapply(seq_len(nrow(df)), function(i) {
    replicate(df[i, ncol(df)], df[i, vars])
  }))
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
#' data <- make_data(model, n = 2)
#' # CausalQueries:::data_type_names(model, data)
#' @keywords internal
#' @noRd

data_type_names <- function(model, data) {
  vars <- model$nodes
  data <- data[vars]
  data[data == ""] <- NA
  out <-
    apply(data, 1, function(j) {
      paste(paste0(vars[!is.na(j)], j[!is.na(j)]), collapse = "")
    })
  out[out == ""] <- "None"
  return(out)
}

