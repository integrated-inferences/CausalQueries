rm(list = ls())

library(gbiqq)



# test_dag <-
# 	gbiqq::make_dag(add_edges(parent = "X",children = c("K", "Y1")),
# 									add_edges(parent = "K", children = c("Y1","Y2")),
# 									add_edges(parent = "Z", children = c("K")))
#
# test_dag <-
# 	gbiqq::make_dag(add_edges(parent = "X",children = c("K", "Y1")),
# 									add_edges(parent = "K", children = c("Y1")),
# 									add_edges(parent = "Z", children = c("Y2")))
#
test_dag <-
	gbiqq::make_dag(add_edges(parent = "X",children = c("K", "Y1")),
									add_edges(parent = "K", children = "M"),
									add_edges(parent = "M", children = "Y1"),
									add_edges(parent = "Z", children = "Y2"))


test_dag <-
	gbiqq::make_dag(add_edges(parent = "X",children = c("K","Y")),
									add_edges(parent = c("K"),children = "Y"))

plot_dag(test_dag)

gbiqq::get_types(test_dag)
gbiqq::get_parents(test_dag)
gbiqq::get_exogenous_vars(test_dag)
gbiqq::get_terminal_vars(test_dag)
gbiqq::get_endogenous_vars(test_dag)
gbiqq::get_variables(test_dag)
# chains approach

gbiqq::get_chains(test_dag)

gbiqq::get_possible_data(test_dag, collapse = FALSE)
gbiqq::get_ambiguities(test_dag)
gbiqq::make_ambiguity_matrices(test_dag)
# gbiqq::collapse_ambiguity_matrices(test_dag)

a <- gbiqq::expand_ambiguity_matrices(test_dag)

dim(a) ==
	c(2^length(gbiqq::get_variables(test_dag)),
		Reduce(x = lapply(gbiqq::get_types(test_dag)[gbiqq::get_endogenous_vars(test_dag)],
											FUN = nrow), f = "*"))

rownames(a)
colnames(a)




# ATTEMPT CREATING L AND P --------------------------------------------------------------------

rm(list = ls())


library(gbiqq)

test_dag <-
	gbiqq::make_dag(add_edges(parent = "X",children = c("K","Y")),
									add_edges(parent = c("K"),children = "Y"))
#
# test_dag <-
# 	gbiqq::make_dag(add_edges(parent = "X",children = c("K", "Y1")),
# 									add_edges(parent = "K", children = c("Y1","Y2")),
# 									add_edges(parent = "Z", children = c("K")))
#
# test_dag <-
# 	gbiqq::make_dag(add_edges(parent = "X",children = c("K", "Y1")),
# 									add_edges(parent = "K", children = c("Y1")),
# 									add_edges(parent = "Z", children = c("Y2")))
#
# test_dag <-
# 	gbiqq::make_dag(add_edges(parent = "X",children = c("K", "Y1")),
# 									add_edges(parent = "K", children = "M"),
# 									add_edges(parent = "M", children = "Y1"),
# 									add_edges(parent = "Z", children = "Y2"))

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


# prelims for our stuff -----------------------------------------------------------------------

types <- sapply(gbiqq::get_types(test_dag), FUN = function(types) dim(types)[1])
types <- types[gbiqq::get_endogenous_vars(test_dag)]

childs <-
	sapply(gbiqq::get_exogenous_vars(test_dag),
				 FUN = function(exogenous_variable) {
				 	names(gbiqq::get_parents(test_dag))[ sapply(X = gbiqq::get_parents(test_dag),
				 																							FUN = function(parents) exogenous_variable %in% parents ) ] },
				 simplify = FALSE
	)

# lambda stuff simulation
#
# In generalized framework this will be asked to be filled in outside STAN program and the length of
# expansion of those vectors will be given as well as priors in expanded form
lambdas <- lapply(types, function(variable_types) {
	# sample(x = seq(0.5, 0.5, by = 0.5), size = variable_types, replace = T)
	lambda_draw <- runif(variable_types)
	return(lambda_draw/sum(lambda_draw))
				 })

# pi stuff
pi <- lapply(childs, FUN = function(childs) types[childs])
pi <- lapply(pi,
						 FUN = function(exogenous_var) {

						 	pos <- which(names(exogenous_var) %in% names(types))
						 	select_vars <- cumsum((seq_along(names(types)) == min(pos)) + (seq_along(names(types)) == (max(pos) + 1)))

						 	# this should be changed to whatever the best way is
						 	temp <- apply(
						 		expand.grid(
						 			lapply(exogenous_var,
						 						 FUN = function(variable_types) {
						 						 	sample(x = seq(0.5, 0.5, by = 0.5), size = variable_types, replace = T)
						 						 }) ),
						 		MARGIN = 1,
						 		FUN = prod)

						 	rep(temp,
						 			times = prod(types[select_vars == 2]),
						 			each = prod(types[select_vars == 0]))

						 })

max_possible_data <-
	Reduce(f = merge,
				 x = get_possible_data(test_dag, collapse = FALSE)[get_terminal_vars(test_dag)])

max_possible_data <- max_possible_data[get_variables(test_dag)]

endogenous_vars <- get_endogenous_vars(test_dag)

# our stuff -----------------------------------------------------------------------------------



A <- gbiqq::expand_ambiguity_matrices(test_dag)

dim(A) ==
	c(2^length(gbiqq::get_variables(test_dag)),
		Reduce(x = lapply(gbiqq::get_types(test_dag)[gbiqq::get_endogenous_vars(test_dag)],
											FUN = nrow), f = "*"))


rownames(A)
colnames(A)


# L construction STAN style (only requires priors and number of variables)

L <- lambdas[[length(endogenous_vars)]]

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

# L construction sanity check one-liner
# L <- apply(expand.grid(lambdas), MARGIN = 1, FUN = prod)

P <- matrix(1, nrow = nrow(A), ncol = ncol(A))

for (exogenous_var in get_exogenous_vars(test_dag)) {
	P <- P *
		max_possible_data[,exogenous_var] %*% t(pi[[exogenous_var]]) +
		(1 - max_possible_data[,exogenous_var]) %*% t(1 - pi[[exogenous_var]])
}


w <- L %*% t(A*P)
sum(w)


# Likelihood using w ------------------------------------------------------

likelihood_helpers <- get_likelihood_helpers(test_dag)

w_starts <- likelihood_helpers$w_starts
w_ends <- likelihood_helpers$w_ends
A_w <- likelihood_helpers$A_w
n_strategies <- likelihood_helpers$n_strategies


# Expand w to be big

w_all <- A_w %*% t(w)


# Likelihood will be a loop like this:
for(i in 1:n_strategies){
	print(sum(w_all[w_starts[i]:w_ends[i]]))
}



















