library(rstan)


rstan_options(auto_write = TRUE)


# BASE MODEL SIMPLEX --------------------------------------------------------------------------

# test <- stan(file = "other/test_multinomial.stan",
# 						 data = list(
# 						 	K = 10,
# 						 	x = 2 * rmultinom(1, size = 20, prob = 1:10 / 10)[,1]))
#
# test


# RAGGED SIMPLEX EXTENSION --------------------------------------------------------------------

# K - number of vectors in vector of vectors (say lambdas)
# K_each - lengths of vectors in vector of vectors (say lambdas)
# N - length of vector of vectors (say lambdas)
# dirichlet_prior - alphas for dirichlet priors (length N)
# starts/ends - starting/ending positions of vectors in vector of vectors (say lambdas)
# Y - observed counts for multinomial distribution (length N)


test <- stan(file = "other/ragged_simplex.stan",
						 data = list(
						 	K = 4,
						 	K_each = c(5,5,3,20),
						 	N = sum(c(5,5,3,20)),
						 	dirichlet_prior = rep(1, 33),
						 	starts = c(1,6,11,14),
						 	ends = cumsum(c(5,5,3,20)),
						 	Y = 2 * rmultinom(1, size = 20, prob = 1:33 / 50)[,1]
	))

test
