#' Integrate gbiqq with DeclareDesign
#'
#' Define a model, estimand, and answer strategy
#'
#' @param model A model created by make_model()
#' @param data_strat should be of the form "list(n_obs = n_obs, vars = list("X", "Y"), probs = list(...), n = NULL, subsets = list(...))
#' @import DeclareDesign
#' @importFrom magrittr %>%
#' @export
#' @examples
#' require("DeclareDesign")
#' my_design <- gbiqq_designer(
#'   model = make_model("X -> Y"),
#'   parameters = c(.5, .5, .1, .7, .1, .1),  # Truth
#'   data_strat = list(n_obs = 5, vars = list(NULL), probs = list(NULL), ns = NULL,
#'      subsets = list(NULL)),
#'   queries = list(ATE = "Y[X=1] - Y[X=0]"))
#' draw_data(my_design)
#' draw_estimands(my_design)
#' draw_estimates(my_design)
#' # diagnose_design(my_design, sims = 2)

gbiqq_designer <- function(
	model = make_model("X -> Y"),
	restrictions = NULL,
	priors = "uniform",
	parameters = NULL,        # True parameters
	data_strat = NULL,
	queries = list(ATE = "Y[X=1] - Y[X=0]")
) {

	model <- 	model %>%
			set_restrictions(restrictions) %>%
			set_priors(prior_distribution = priors) %>%
	   	set_prior_distribution()

	if(is.null(parameters)) {message("No true parameters provided; parameters drawn from prior on each run")}

	data_step <- declare_population(data =
									data_strategy(
						    				model,
						    				parameters = parameters,
										    n_obs = data_strat$n_obs,
										    vars = data_strat$vars,
										    probs = data_strat$probs,
										    n = data_strat$n,
										    subsets = data_strat$subsets))

	# Estimand given parameters
	estimand <- declare_estimand(handler = function(data) {
		value <- gbiqq::get_estimands(model,
													 using = "parameters",
													 parameters = parameters,
													 queries = queries)
		data.frame(estimand_label = names(queries),
							 estimand = value["mean",],
							 stringsAsFactors = FALSE)})

	# Estimator runs gbiqq
	 estimate <- declare_estimator(handler = function(data) {
		updated <- gbiqq(model = model,  data = data)
		value   <- gbiqq::get_estimands(updated, using = "posteriors", queries = queries)
		data.frame(estimate_label = paste0("est_", names(queries)),
							 estimand = names(queries),
							 estimate = value["mean",],
							 sd_estimate = value["sd",], stringsAsFactors = FALSE)
	 })

	# Declare design
	data_step + estimand + estimate

}

