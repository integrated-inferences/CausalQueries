rm(list = ls())
library(gbiqq)

dag <-
	gbiqq::make_dag(add_edges(parent = "X",children = c("K", "Y")),
									add_edges(parent = c("K"),children = "Y"))


A <- gbiqq::collapse_ambiguity_matrices(dag)$Y_K

get_types(dag)

# Can Y type 1 and K type 4 produce 010, assuming: Y = 0, X = 1 , K = 0
# Y type 1:
# 00 01 10 11
# 0  0  0  0
# K type 4:
# 0 1
# 1 1


# Parameters --------------------------------------------------------------

set.seed(10)
# Parameters of interest: share of Y causal types in population
lambda_Y <- runif(16)
lambda_Y <- lambda_Y/sum(lambda_Y)
# Parameters of interest: share of K causal types in population
lambda_K <- runif(4)
lambda_K <- lambda_K/sum(lambda_K)
# pi for each primitive value of Y, K
pi <- matrix(data = runif( 16 * 4 ), nrow = 16)


# MH BIQQ Code ------------------------------------------------------------


# Generate matrix of types
causal_type <- function(iK, iY){
	Y0 <- lambda_Y_types[iY, 1 + lambda_K_types[iK,1]]     #X=0, K= K(0)
	Y1 <- lambda_Y_types[iY, 3 + lambda_K_types[iK,2]]
	type <- 1 + (Y0 + 2*Y1)
	(c(3,1,2,4))[type]
}
lambda_Y_types <- matrix(perm(rep(2,4)), 16, 4)
lambda_K_types <- matrix(perm(rep(2,2)), 4, 2)
types <- t(sapply(1:16, function(iY){sapply(1:4, function(iK) {causal_type(iK, iY)})} ))


# Parameters
w_XKY <- rep(0,8)

for (i in 1:16) {
	for (j in 1:2)  {
		# // X = 0, K = 0, Y = 0
		if (types[i,j] == 2  || types[i,j] == 3 ) w_XKY[1] = w_XKY[1] + (lambda_Y[i]*(1-pi[i,j])*lambda_K[j]);

		# // X = 0, Y = 1, K = 0
		if(types[i,j] == 1  || types[i,j] == 4 ) w_XKY[3] = w_XKY[3] + (lambda_Y[i]*(1-pi[i,j])*lambda_K[j]);

		# // X = 1, Y = 0, K = 0
		if(types[i,j] == 1  || types[i,j] == 3 )  w_XKY[5]  =  w_XKY[5] + (lambda_Y[i]*(pi[i,j])*lambda_K[j]);

		# // X = 1, K = 0  , Y = 1
		if(types[i,j] == 2  || types[i,j] == 4 ) w_XKY[7]  =  w_XKY[7] + (lambda_Y[i]*(pi[i,j])*lambda_K[j]);
	}

	for (j in 3:4)  {
		if(types[i,j] == 2  || types[i,j] == 3 ) w_XKY[2] = w_XKY[2] + (lambda_Y[i]*(1-pi[i,j])*lambda_K[j]);

		# // X = 0, Y = 1, K = 1
		if(types[i,j] == 1  || types[i,j] == 4 ) w_XKY[4]  =  w_XKY[4] + (lambda_Y[i]*(1-pi[i,j])*lambda_K[j]);

		# // X = 1, K = 1, Y = 0
		if(types[i,j] == 1  || types[i,j] == 3 ) w_XKY[6]  =  w_XKY[6] + (lambda_Y[i]*(pi[i,j])*lambda_K[j]);

		# // X = 1, K = 1, Y = 1
		if(types[i,j] == 2  || types[i,j] == 4 ) w_XKY[8]  =  w_XKY[8] + (lambda_Y[i]*(pi[i,j])*lambda_K[j]);
	}}

# Test --------------------------------------------------------------------

# Our matrix algebra using pi, lambda and A should produce w_XKY






