#' Observe data, given a strategy
#'
#' @param complete_data A data.frame. Data observed and unobserved.
#' @param observed A data.frame. Data observed.
#' @param vars_to_observe  A list of variables to observe.
#' @param prob A scalar. Observation probability.
#' @param m Number of units to observe; if specified, \code{m} overrides \code{prob}.
#' @param subset A logical statement that can be applied to rows of complete data. For instance observation fo some variables might depend on observed values of other variables; or observation may only be sought if data not already observed!
#'
#' @export
#' @examples
#' model <- make_model("X -> Y")
#' df <- simulate_data(model, n = 8)
#' # Observe X values only
#' observe(complete_data = df, vars_to_observe = "X")
#' # Observe half the Y values for cases with observed X = 1
#' observe(complete_data = df,
#'      observed = observe(complete_data = df, vars_to_observe = "X"),
#'      vars_to_observe = "Y", prob = .5,
#'      subset = "X==1")

# A strategy consists of a. names of types to reveal  b. number of these to reveal c. subset from which to reveal them

observe <- function(complete_data,
                    observed = NULL,
                    vars_to_observe = NULL,
                    prob = 1,
                    m = NULL,
                    subset = NULL){
  
  if(is.null(observed)) {observed <- complete_data; observed[,] <- FALSE}
  if(is.null(vars_to_observe)) vars_to_observe <- names(complete_data)
  
  observed_data <- complete_data
  observed_data[!observed] <- NA
  
  # Handle cases with no subsetting; where condition is empty, and when the condition is satisfied
  
  if(is.null(subset)){ observed[, vars_to_observe] <- TRUE
  
  }  else {
    
    strata <- with(observed_data, eval(parse(text = subset)))
    
    if(max(strata) == 1){
      
      if(!is.null(m)) prob <- min(1, m/sum(strata))   # If m is specified, use this to extent possible
      
      show <- randomizr::strata_rs(strata = strata,
                                   strata_prob = c(0, prob)) == 1
      observed[show, vars_to_observe] <- TRUE
    }}
  
  observed
}
