rm(list = ls())

library(gbiqq)


# Build dag ---------------------------------------------------------------

test_dag <-
	gbiqq::make_dag(add_edges(parent = "X",children = c("K","Y")),
									add_edges(parent = c("K"),children = "Y"))

# test_dag <-
# 	gbiqq::make_dag(add_edges(parent = "X",children = c("K", "Y1")),
# 									add_edges(parent = "K", children = c("Y1","Y2")),
# 									add_edges(parent = "Z", children = c("K")))


# Inspect DAG ---------------------------------------------------------------------------------

# look at the DAG
plot_dag(test_dag)

# basics
gbiqq::get_types(test_dag)
gbiqq::get_parents(test_dag)
gbiqq::get_exogenous_vars(test_dag)
gbiqq::get_terminal_vars(test_dag)
gbiqq::get_endogenous_vars(test_dag)
gbiqq::get_variables(test_dag)

# chains approach
gbiqq::get_chains(test_dag)

# reimplemented functions
gbiqq::get_possible_data(test_dag, collapse = FALSE)
gbiqq::get_ambiguities(test_dag)
gbiqq::make_ambiguity_matrices(test_dag)

# setup priors for generalized BIQQ ------------------------------------------------------------


# lambda priors
# (In generalized framework this will be asked to be filled in outside STAN program and the length of
# expansion of those vectors will be given as well as priors in expanded form)
n_endogenous_types <- get_n_endogenous_types(test_dag)
(
	lambdas <- lapply(n_endogenous_types, function(variable_types) {
		lambda_draw <- runif(variable_types)
		return(lambda_draw/sum(lambda_draw))
	})
)

# pi priors
# (In generalized framework this will be asked to be filled in outside STAN program and the length of
# expansion of those vectors will be given as well as priors in expanded form)
children <- get_children_of_exogenous(test_dag)
( pi <- lapply(children, FUN = function(childs) n_endogenous_types[childs]) )
(
	pis <- lapply(pi,
								FUN = function(exogenous_var) {
									rep(.5, times = prod(exogenous_var))
								})
)

# important object which allows to expand all pi priors to the same dimensionality as A matrix
pi_times_each <- get_pi_expanders(pi = pi, dag = test_dag)

# a bunch of useful objects
max_possible_data <- get_max_possible_data(test_dag)
endogenous_vars <- get_endogenous_vars(test_dag)


# STAN Stuff -----------------------------------------------------------------------------------

A <- gbiqq::expand_ambiguity_matrices(test_dag)


# L construction STAN style
# (only requires priors and number of variables)

L <- lambdas[[length(endogenous_vars)]]

# Fix loop to work with lambda = vector instead of lambda = list
for (variable in (length(lambdas) - 1):1) {

	L_temp <- rep(NA, times = length(L)*length(lambdas[[variable]]))
	k <- 1

	for (i in 1:length(L)) {
		for (j in 1:length(lambdas[[variable]])) {
			L_temp[k] <- L[i] * lambdas[[variable]][j]
			k <- k + 1
		}
	}

	L <- L_temp
}


# P construction STAN style
# (This requires only times/each object for each exogenous variable to bring only relevant endogenous vars
# to conformable dimensions and requires all relevant pi parameters given by user)

P <- matrix(1, nrow = nrow(A), ncol = ncol(A))
# pis and pi_each and pi_times need to work as vectors here
for (exogenous_var in get_exogenous_vars(test_dag)) {

	pi_temp <-
		rep(pis[[exogenous_var]],
				times = pi_times_each[[exogenous_var]]$times,
				each = pi_times_each[[exogenous_var]]$each)

	P <- P * (max_possible_data[,exogenous_var] %*% t(pi_temp) +
							(1 - max_possible_data[,exogenous_var]) %*% t(1 - pi_temp))

}


t( w <- L %*% t(A * P) )

stopifnot(
	all.equal(target = 1, current =  sum(w))
)

# Likelihood using w ------------------------------------------------------

likelihood_helpers <- get_likelihood_helpers(test_dag)

w_starts <- likelihood_helpers$w_starts
w_ends <- likelihood_helpers$w_ends
A_w <- likelihood_helpers$A_w
n_strategies <- likelihood_helpers$n_strategies


# Expand w to be big

w_all <- A_w %*% t(w)


# Likelihood will be a loop like this:
for (i in 1:n_strategies) {
	stopifnot(
		all.equal(target = 1,
							current =  sum(w_all[w_starts[i]:w_ends[i]]))
	)
}











