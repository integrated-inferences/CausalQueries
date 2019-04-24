#' Query a model
#'
#' Ask a question of a model
#' @param model A model created by make_model()
#' @param query A logic operation on variables, in quotes. For example "Y==1"
#' @param subset An optional logic condition on variabless, in quotes.. For example "X==0"
#' @export
#' @examples
#' model <- make_model(add_edges(parent = "X", children = c("Y")))
#' w     <- draw_event_prob(model, P = NULL, A = NULL)
#' query_model(model,  "Y==1", "X==1", w)
#' query_model(model,  "Y==1", "X==1", w) - query_model(model,  "Y==1", "X==0", w)
#'
#'# ATE: Make a model with monotonicity and reasonable prior certainty on uniform types
#' model     <-  make_model(add_edges(parent = "X", children = c("Y")))
#' model     <-  set_priors(model = model, prior_distribution = NULL, alpha =  10)
#' model     <-  reduce_nodal_types(model = model, restrictions = list(Y = "Y10"))
#' model_do0 <-  reduce_nodal_types(model = model, do = list(X = 0))
#' model_do1 <-  reduce_nodal_types(model = model, do = list(X = 1))
#' query_model(model_do1,  "Y==1") - query_model(model_do0,  "Y==1")
#' simulations <- replicate(200, query_model(model_do1,  "Y==1") - query_model(model_do0,  "Y==1"))
#' hist(simulations, main = paste("E(ATE) = ", round(mean(simulations), 2)))
#' # This is not quite right since the *same* prior draw should be used for the two queries -- a bit confusing though
#' # as currently set up since the two models have different parameters! May need another approch to "do":
#' # e.g. draw a parameter vector and set certainty around that, calculated estimand, repeat over different draws
#' model     <-  make_model(add_edges(parent = "X", children = c("Y")))


query_model <- function(model, query, subset = TRUE, w = NULL){

	if(is.null(w)) w <- draw_event_prob(model)

	df <- get_max_possible_data(model)

	a  <- with(df, eval(parse(text = query)))

	if(!is.logical(subset)) subset <- with(df, eval(parse(text = subset)))
	if(!any(subset)) stop("Empty subset")

	weighted.mean(a[subset], w[subset])
}


#' Calculate estimand
#'
#' Ask a question of a model
#' @param model A model created by make_model()
#' @param query A logic operation on variables, in quotes. For example "Y==1"
#' @param subset An optional logic condition on variabless, in quotes.. For example "X==0"
#' @export
#' @examples
#' model <- make_model(add_edges(parent = "X", children = c("Y")))
#' model <- reduce_nodal_types(model = model, restrictions = list(Y = "Y10"))
#' estimand <- calculate_estimand (model = model,
#' 										do1 = list(X = 0),
#' 										query1 = "Y==1",
#' 										do2 = list(X = 1),
#' 										query2 = "Y==1",
#' 										aggregation = function(q1,q2) q2-q1,
#' 										sims = 300)
#' summary(estimand)

calculate_estimand <- function(model,
															 do1, query1,
															 do2 = NULL, query2 = NULL,
															 subset1 = TRUE, subset2 = TRUE,
															 aggregation = function(a,b) a-b,
															 sims = 100) {

	f <- function() {
	  m  <-  set_priors(model = model, alpha = 10000*draw_lambda(model))
	  aggregation(
	  query_model(reduce_nodal_types(model = m, do = do1),  query1, subset1),
	  query_model(reduce_nodal_types(model = m, do = do2),  query2, subset2))
	  }
	out <- replicate(sims, f())
	out

}
