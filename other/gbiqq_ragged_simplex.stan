data {

	int<lower=1> K; // number of variables
	int<lower=1> K_endog; // number of exogenous variables
  int<lower=1> K_endog_each[K_endog]; // number of types for each exogenous variable
	int<lower=1> N_types; // sum(K_endog_each)
	int<lower=1> N_expand; // prod(K_endog_each)
	int<lower=1> N_strategies;
	int<lower=1> N_data;
	vector<lower=0>[N_types] dirichlet_prior;
  int<lower=1> l_starts[K_endog];
  int<lower=1> l_ends[K_endog];

	matrix[N_data,K] max_data;
	matrix[N_data,N_expand] A;
	int<lower=0,upper=2147483647> Y[N_strategies]; // incorrect dimensionality, only for the tryout

}
parameters {

	vector<lower=0>[N_types-K_endog] gamma;

}
transformed parameters {

	vector<lower=0>[N_types] lambdas_base;
	vector<lower=1>[K_endog] sum_gammas;
	for (i in 1:K) {
		sum_gammas[i] = 1 + sum(gamma[(l_starts[i] - (i-1)):(l_ends[i] - i)]);
		lambdas_base[l_starts[i]:l_ends[i]] = append_row(1,gamma[(l_starts[i] - (i-1)):(l_ends[i] - i)]) / sum_gammas[i];
	}

}
model {

	// expanded vector of lambdas (L)
	vector[N_expand] lambdas;

	// expansion size (will be changed in the loop for L)
	int expansion = K_endog_each[K_endog];

	// fill in L with only lambda draws for last endogenous variable (will be changed in the loop for L)
	lambdas[(N_expand - K_endog_each[K_endog]):N_expand] = lambdas_base[l_starts[K_endog]:l_ends[K_endog]];

	// fill in L for the rest of endogenous variables in a BACKWARDS order
	for (i in (K_endog - 1):1) {

		// temproary L fill in
		vector[expansion*K_endog_each[i]] lambdas_temp;

		int m = 1;
		for (j in (N_expand-expansion):N_expand) { // j goes from first to last element position of previous expansion
			for (k in l_starts[i]:l_ends[i]) { // k goes from first to last element position for endog variable i in lambdas_base
				lambdas_temp[m] = lambdas[j] * lambdas_base[k];
				m = m + 1;
			}
		}

		// adjust expansion for the next iteration of the loop
		expansion = expansion * K_endog_each[i];
		// fill in L with temporary L fill in
		lambdas[(N_expand-expansion):N_expand] = lambdas_temp;
	}

	// priors
	target += gamma_lpdf(lambdas_base | dirichlet_prior, 1);

	for (i in 1:K_endog) {
		target += -K_endog_each[i] * log(sum_gammas[i]);
		target += multinomial_lpmf(Y[l_starts[i]:l_ends[i]] | lambdas[l_starts[i]:l_ends[i]]);
	}
}
