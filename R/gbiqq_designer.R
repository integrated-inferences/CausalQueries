
gbiqq_designer <- function(
	model = make_model("X" %->% "Y"),
	restrictions = NULL,
	priors = "uniform",
	data_strat = list(n = 1,vars = list(NULL), probs = list(NULL), ms = NULL, subsets = list(NULL)),
	queries = list(ATE = "Y[X=1] - Y[X=0]")
) {


model <- 	model %>%
		set_restrictions(restrictions) %>%
		set_priors(lambda_priors = priors) %>%
		set_parameter_matrix()


data_step <- declare_population(handler =
								data_strategy(
									model,
								      n = data_strat$n,
								      vars = data_strat$vars,
								      probs = data_strat$probs,
								      ms = data_strat$ms,
								      subsets = data_strat$subsets))

}
