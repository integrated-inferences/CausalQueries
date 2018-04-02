rm(list = ls())

library(gbiqq)

## PREPARE DATA FOR STAN



# Generate DAG --------------------------------------------------------------------------------

# test_dag <-
# 	gbiqq::make_dag(add_edges(parent = "X",children = c("K","Y")),
# 									add_edges(parent = c("K"),children = "Y"))

test_dag <-
	gbiqq::make_dag(add_edges(parent = "X", children = c("K", "Y1")),
									add_edges(parent = "K", children = c("Y1","Y2")),
									add_edges(parent = "Z", children = c("Y2")))

# look at the DAG
plot_dag(test_dag)

(
	L_alpha <-
		lambdas <-
		do.call(c,
						lapply(gbiqq::get_n_endogenous_types(test_dag), function(variable_types) {
							lambda_draw <- rep(1/variable_types, times = variable_types)
							return(lambda_draw)
						})
		)
)

(
	P_alpha <- P_beta <-
		pi <-
		do.call(c,
						lapply(
							lapply(gbiqq::get_children_of_exogenous(test_dag),
										 FUN = function(childs) gbiqq::get_n_endogenous_types(test_dag)[childs]),
							FUN = function(child_types) rep(.5, times = prod(child_types))
						)
		)
)

# Prep data for STAN --------------------------------------------------------------------------

n_endog <- length(gbiqq::get_endogenous_vars(test_dag))
n_exog <- length(gbiqq::get_exogenous_vars(test_dag))
n_endogenous_types <- gbiqq::get_n_endogenous_types(test_dag)
n_strategies <- gbiqq::get_likelihood_helpers(test_dag)$n_strategies

max_possible_data <- gbiqq::get_max_possible_data(test_dag)

# for A (ambiguity)

A <- gbiqq::expand_ambiguity_matrices(test_dag)

# for L (lambda)

l_starts <-
	cumsum(n_endogenous_types) - n_endogenous_types + 1

l_ends <-
	cumsum(n_endogenous_types)

# for P (pi)

.pi_temp <-
		sapply(
			lapply(gbiqq::get_children_of_exogenous(test_dag),
						 FUN = function(childs) gbiqq::get_n_endogenous_types(test_dag)[childs]),
			FUN = function(child_types) prod(child_types)
		)

pi_starts <- cumsum(.pi_temp) - .pi_temp + 1
pi_ends <- cumsum(.pi_temp)

# important object which allows to expand all pi priors to the same dimensionality as A matrix
.pi_temp <-
	get_pi_expanders(pi = lapply(gbiqq::get_children_of_exogenous(test_dag),
															 FUN = function(childs) gbiqq::get_n_endogenous_types(test_dag)[childs]),
									 dag = test_dag)

pi_times <- sapply(.pi_temp, function(v) v$times)
pi_each <- sapply(.pi_temp, function(v) v$each)

