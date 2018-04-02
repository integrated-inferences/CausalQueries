rm(list = ls())

library(gbiqq)

## PREPARE DATA FOR STAN

test_dag <-
	gbiqq::make_dag(add_edges(parent = "X",children = c("K","Y")),
									add_edges(parent = c("K"),children = "Y"))

# test_dag <-
# 	gbiqq::make_dag(add_edges(parent = "X", children = c("K", "Y1")),
# 									add_edges(parent = "K", children = c("Y1","Y2")),
# 									add_edges(parent = "Z", children = c("Y2")))

# look at the DAG
plot_dag(test_dag)

# User supplied stuff --------------------------------------------------------------------------------



(
	lambdas_prior <-
		lambdas <-
		do.call(c,
						lapply(gbiqq::get_n_endogenous_types(test_dag), function(variable_types) {
							lambda_draw <- rep(1/variable_types, times = variable_types)
							return(lambda_draw)
						})
		)
)

pis_prior <-
	do.call(c,
					lapply(
						lapply(gbiqq::get_children_of_exogenous(test_dag),
									 FUN = function(childs) gbiqq::get_n_endogenous_types(test_dag)[childs]),
						FUN = function(child_types) rep(.5, times = prod(child_types))
					)
	)

( pis_prior <- cbind(pis_prior,pis_prior) )

data <- data.frame(
	X = rbinom(100,1,.5),
	Y = rbinom(100,1,.5),
	K = c(rbinom(25,1,.5),rep(NA,75))
)

# Prep data for STAN --------------------------------------------------------------------------

likelihood_helpers <- get_likelihood_helpers(test_dag)
max_possible_data <- gbiqq::get_max_possible_data(test_dag)
children_of_exogenous <- gbiqq::get_children_of_exogenous(test_dag)

n_endog <- length(gbiqq::get_endogenous_vars(test_dag))
n_exog <- length(gbiqq::get_exogenous_vars(test_dag))
n_endogenous_types <- gbiqq::get_n_endogenous_types(test_dag)
n_strategies <- likelihood_helpers$n_strategies

# for A (ambiguity)

A <- gbiqq::expand_ambiguity_matrices(test_dag)

# for L (lambda)

# for the vector of vectors of draws from the prior distributions of lambda
l_starts <- cumsum(n_endogenous_types) - n_endogenous_types + 1
l_ends <- cumsum(n_endogenous_types)

n_lambdas <- max(l_ends)

# for P (pi)

# for the vector of vectors of draws from the prior distributions of pis
.p_temp <-
		sapply(
			lapply(children_of_exogenous,
						 FUN = function(childs) n_endogenous_types[childs]),
			FUN = function(child_types) prod(child_types)
		)

p_starts <- cumsum(.p_temp) - .p_temp + 1
p_ends <- cumsum(.p_temp)

# for expanding this to conform with A and L
.p_temp <-
	get_pi_expanders(pi = lapply(children_of_exogenous,
															 FUN = function(childs) n_endogenous_types[childs]),
									 dag = test_dag)

p_times <- sapply(.pi_temp, function(v) v$times)
p_each <- sapply(.pi_temp, function(v) v$each)

n_pis <- max(p_ends)

# for w (vector of vectors of weights)

w_starts <- likelihood_helpers$w_starts
w_ends <- likelihood_helpers$w_ends
A_w <- likelihood_helpers$A_w

# for Y

Y <- get_data_events(data = data, dag = test_dag)$data_events
