#' Integrate gbiqq with DeclareDesign
#'
#' Define a model, estimand, and answer strategy
#'
#' @param model A model created by make_model()
#'
#' @export
#' @examples
#' my_design <- gbiqq_designer(
#'   model = make_model("X" %->% "Y"),
#'   lambda = c(.5, .5, .1, .7, .1, .1),  # Truth
#'   data_strat = list(n = 4, vars = list(NULL), probs = list(NULL), ms = NULL, subsets = list(NULL)),
#'   queries = list(ATE = "Y[X=1] - Y[X=0]"))
#'   draw_data(my_design)
#'   draw_estimands(my_design)
#'   draw_estimates(my_design)

gbiqq_designer <- function(
	model = make_model("X" %->% "Y"),
	restrictions = NULL,
	priors = "uniform",
	lambda = NULL,        # True parameters
	data_strat = list(n = 1,vars = list(NULL), probs = list(NULL), ms = NULL, subsets = list(NULL)),
	queries = list(ATE = "Y[X=1] - Y[X=0]")
) {

	model <- 	model %>%
			set_restrictions(restrictions) %>%
			set_priors(prior_distribution = priors) %>%
			set_parameter_matrix() %>%
	   	set_prior_distribution()

	if(is.null(lambda)) {message("No true lambda provided; lambda drawn from prior on each run")}

	data_step <- declare_population(data =
									data_strategy(
						    				model,
						    				lambda = lambda,
										    n = data_strat$n,
										    vars = data_strat$vars,
										    probs = data_strat$probs,
										    ms = data_strat$ms,
										    subsets = data_strat$subsets))

	# Estimand given lambda
	estimand <- declare_estimand(handler = function(data) {
		value <- calculate_multiple_estimands(model, lambda = lambda, queries = queries)
		data.frame(estimand_label = names(queries), estimand = value[1,])})

	# Estimator runs gbiqq
	 estimate <- declare_estimator(handler = function(data) {
		updated <- gbiqq(model = model,  data = data)
		calculate_multiple_estimands(updated, posterior = TRUE, queries = queries)
		data.frame(estimate_label = paste0("est_", names(queries)),
							 estimand = names(queries),
							 estimate = value[1,],
							 sd_estimate = value[2,])
	 })

	# Declare a design
	data_step + estimand + estimate

}

