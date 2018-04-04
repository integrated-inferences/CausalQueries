rm(list = ls())

library(gbiqq)


# USER SUPPLIED --------------------------------------------------------------------------------

## PREPARE DATA FOR STAN

# Canonical BIQQ
test_dag <-
	gbiqq::make_dag(add_edges(parent = "X",children = c("K","Y")),
									add_edges(parent = c("K"),children = "Y"))

data <- data.frame(
	X = rbinom(100,1,.5),
	Y = rbinom(100,1,.5),
	K = c(rbinom(25,1,.5),rep(NA,75))
)

# # More complex example
# test_dag <-
# 	gbiqq::make_dag(add_edges(parent = "X", children = c("K", "Y1")),
# 									add_edges(parent = "K", children = c("Y1","Y2")),
# 									add_edges(parent = "Z", children = c("Y2")))
#
# data <- data.frame(
# 	X = rbinom(100,1,.5),
# 	Z = rbinom(100,1,.5),
# 	Y1 = rbinom(100,1,.5),
# 	Y2 = rbinom(100,1,.5),
# 	K = c(rbinom(25,1,.5),rep(NA,75))
# )

# look at the DAG
plot_dag(test_dag)


# lambda priors in correct dimensionality
lambdas_prior <-
	lambdas <-
	do.call(c,
					lapply(gbiqq::get_n_endogenous_types(test_dag), function(variable_types) {
						lambda_draw <- rep(1/variable_types, times = variable_types)
						return(lambda_draw)
					})
	)

# pi priors in correct dimensionality
pis_prior <-
	do.call(c,
					lapply(
						lapply(gbiqq::get_children_of_exogenous(test_dag),
									 FUN = function(childs) gbiqq::get_n_endogenous_types(test_dag)[childs]),
						FUN = function(child_types) rep(.5, times = prod(child_types))
					)
	)

pis_prior <- cbind(pis_prior,pis_prior)




# OUTSIDE STAN --------------------------------------------------------------------------

# int<lower=1> K; // number of variables
# int<lower=1> K_endog; // number of endogenous variables
# int<lower=1> K_exog; // number of exogenous variables
# int<lower=1> N_endog_each[K_endog]; // number of types for each endogenous variable
# int<lower=1> N_types; // sum(N_endog_each)
# int<lower=1> N_events; // number of possible events
# int<lower=1> N_data; // number of possible data realizations
# int<lower=1> N_endog_expand; // prod(N_endog_each)
# matrix<lower=0,upper=1>[N_data,K] max_possible_data;
#
# // lambda data
# vector<lower=0>[N_types] dirichlet_prior;
# int<lower=1> l_starts[K_endog];
# int<lower=1> l_ends[K_endog];
#
# // pi data
# int<lower=1> N_exog_types; //
# real<lower=0> beta_prior[N_exog_types,2];
# int<lower=1> p_starts[K_exog];
# int<lower=1> p_ends[K_exog];
# int<lower=1> p_each[K_exog];
# int<lower=1> p_times[K_exog];
#
# // ambiguity data
# matrix<lower=0,upper=1>[N_data,N_endog_expand] A;
#
# // user data
# int<lower=0,upper=2147483647> Y[N_events]; // incorrect dimensionality, only for the tryout

# preliminary
likelihood_helpers <- get_likelihood_helpers(test_dag)
children_of_exogenous <- gbiqq::get_children_of_exogenous(test_dag)

### Driect analogues of STAN data

K <- n <- length(gbiqq::get_variables(test_dag))
K_endog <- n_endog <-  length(gbiqq::get_endogenous_vars(test_dag))
K_exog <- n_exog <- length(gbiqq::get_exogenous_vars(test_dag))
N_endog_each <- n_endogenous_types <- gbiqq::get_n_endogenous_types(test_dag)
N_types <- n_lambdas <- sum(N_endog_each)
# for now, this is generally incorrect
N_events <-
	length(likelihood_helpers$possible_events[which(names(likelihood_helpers$possible_events) == "XKY")])
N_data <- nrow(gbiqq::get_max_possible_data(test_dag))
N_endog_expand <- prod(N_endog_each)
max_possible_data <- gbiqq::get_max_possible_data(test_dag)

# checks
stopifnot(dim(max_possible_data) == c(N_data,K))

### lambda data

dirichlet_prior <- lambdas_prior
l_starts <- cumsum(n_endogenous_types) - n_endogenous_types + 1
l_ends <- cumsum(n_endogenous_types)

# checks
stopifnot(any(length(dirichlet_prior) == N_types,
							length(l_starts) == K_endog,
							length(l_starts) == K_endog))

### pi data

# for the vector of vectors of draws from the prior distributions of pis
.p_temp <-
	sapply(
		lapply(children_of_exogenous,
					 FUN = function(childs) N_endog_each[childs]),
		FUN = function(child_types) prod(child_types)
	)


N_exog_each <- .p_temp
N_exog_types <- n_pis <- sum(.p_temp)
beta_prior <- pis_prior
p_starts <- cumsum(.p_temp) - .p_temp + 1
p_ends <- cumsum(.p_temp)

# for expanding this to conform with A and L
.p_temp <-
	get_pi_expanders(pi = lapply(children_of_exogenous,
															 FUN = function(childs) N_endog_each[childs]),
									 dag = test_dag)

p_times <- sapply(.p_temp, function(v) v$times)
p_each <- sapply(.p_temp, function(v) v$each)

# checks
stopifnot(any(length(dirichlet_prior) == N_types,
							length(p_starts) == K_exog,
							length(p_ends) == K_exog,
							length(l_starts) == K_exog,
							length(l_starts) == K_exog,
							length(N_exog_each) == K_exog))

### for A (ambiguity)

A <- gbiqq::expand_ambiguity_matrices(test_dag)


### for w (vector of vectors of weights) -- not implemented yet

# w_starts <- likelihood_helpers$w_starts
# w_ends <- likelihood_helpers$w_ends
# A_w <- likelihood_helpers$A_w

### for Y (vector of data counts)

Y <- get_data_events(data = data, dag = test_dag)$data_events$count[1:2^K]



# WITHIN STAN Stuff -----------------------------------------------------------------------------------


lambdas_base <- lambdas_prior # to simplify things

# lambdas construction STAN style
lambdas <- rep(NA, N_endog_expand) # no need to do this in STAN
expansion <- N_endog_each[K_endog]

lambdas[(N_endog_expand - N_endog_each[K_endog] + 1):N_endog_expand] = lambdas_base[l_starts[K_endog]:l_ends[K_endog]]

for (i in (K_endog - 1):1) {

	lambdas_temp <- rep(NA, expansion*N_endog_each[i]) # no need to do this in STAN

	m = 1
	for (j in (N_endog_expand - expansion + 1):N_endog_expand) { # j goes from first to last element position of previous expansion
		for (k in l_starts[i]:l_ends[i]) { # k goes from first to last element position for endog variable i in lambdas_base
			lambdas_temp[m] = lambdas[j] * lambdas_base[k]
			m = m + 1
		}
	}

	expansion = expansion * N_endog_each[i]
	lambdas[(N_endog_expand - expansion + 1):N_endog_expand] = lambdas_temp
}


# for (i in 1:K_exog) {
#
# 	vector[N_endog_expand] pis_temp;
# 	vector[N_exog_types*p_each[i]] pis_temp_each;
#
# 	int pos = 1;
# 	for (j in 1:N_exog_types) {
# 		pis_temp_each[pos:(pos+p_each[i]-1)] = rep_vector(pis_base[j], p_each[i]);
# 		pos = pos + p_each[i];
# 	}
#
# 	pis_temp[1:(N_exog_types*p_each[i]-1)] = pis_temp_each;
#
# 	for (k in 1:(p_times[i]-1)) {
# 		pis_temp = append_row(pis_temp,pis_temp_each);
# 	}
#
# 	pis = pis * (max_possible_data[,i] * pis_temp' +
# 							 (1 - max_possible_data[,i]) * (1 - pis_temp)');
#
# }

pis_base <- beta_prior[,1] # to simplify things
pis = matrix(1, nrow = N_data, ncol = N_endog_expand)

for (i in 1:K_exog) {

	pis_temp <- rep(NA, N_endog_expand)
	pis_temp_each <- rep(NA, N_exog_each[i]*p_each[i])

	pos = 1
	for (j in 1:N_exog_each[i]) {
		pis_temp_each[pos:(pos + p_each[i] - 1)] = rep(pis_base[j], p_each[i]);
		pos = pos + p_each[i];
	}

	pis_temp[1:(N_exog_each[i]*p_each[i])] = pis_temp_each

	counter = p_times[i]
	while (counter > 1) {
		pis_temp = c(pis_temp,pis_temp_each)
		counter = counter - 1
	}

	pis = pis * (max_possible_data[,i] %*% t(pis_temp) +
								(1 - max_possible_data[,i]) %*% t(1 - pis_temp))

}

# children <- get_children_of_exogenous(test_dag)
# ( pi <- lapply(children, FUN = function(childs) n_endogenous_types[childs]) )
# (
# 	pis <- lapply(pi,
# 								FUN = function(exogenous_var) {
# 									rep(.5, times = prod(exogenous_var))
# 								})
# )
# # important object which allows to expand all pi priors to the same dimensionality as A matrix
# pi_times_each <- get_pi_expanders(pi = pi, dag = test_dag)
#
#
# P <- matrix(1, nrow = nrow(A), ncol = ncol(A))
# # pis and pi_each and pi_times need to work as vectors here
# for (exogenous_var in get_exogenous_vars(test_dag)) {
#
# 	pi_temp <-
# 		rep(pis[[exogenous_var]],
# 				times = pi_times_each[[exogenous_var]]$times,
# 				each = pi_times_each[[exogenous_var]]$each)
#
# 	P <- P * (max_possible_data[,exogenous_var] %*% t(pi_temp) +
# 							(1 - max_possible_data[,exogenous_var]) %*% t(1 - pi_temp))
#
# }


t( w <- lambdas %*% t(A * pis) )

stopifnot(
	all.equal(target = 1, current =  sum(w))
)


# Run STAN ------------------------------------------------------------------------------------

test <- stan(file = "other/gbiqq_ragged_simplex.stan",
						 data = list(
						 	K = K,
						 	K_endog = K_endog,
						 	K_exog = K_exog,
						 	N_endog_each = N_endog_each,
						 	N_types = N_types,
						 	N_events = N_events,
						 	N_data = N_data,
						 	N_endog_expand = N_endog_expand,
						 	max_possible_data = max_possible_data,
						 	dirichlet_prior = dirichlet_prior,
						 	l_starts = l_starts,
						 	l_ends = l_ends,
						 	N_exog_each = N_exog_each,
						 	N_exog_types = N_exog_types,
						 	beta_prior = beta_prior,
						 	p_starts = p_starts,
						 	p_ends = p_ends,
						 	p_times = p_times,
						 	p_each = p_each,
						 	A = A,
						 	Y = Y
						 ))

test
