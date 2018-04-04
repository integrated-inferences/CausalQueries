data {

	int<lower=1> K; // number of variables
	int<lower=1> K_endog; // number of endogenous variables
	int<lower=1> K_exog; // number of exogenous variables
  int<lower=1> N_endog_each[K_endog]; // number of types for each endogenous variable
	int<lower=1> N_types; // sum(N_endog_each)
	int<lower=1> N_events; // number of possible events
	int<lower=1> N_data; // number of possible data realizations
	int<lower=1> N_endog_expand; // prod(N_endog_each)
	matrix<lower=0,upper=1>[N_data,K] max_possible_data;

	// lambda data
	vector<lower=0>[N_types] dirichlet_prior;
  int<lower=1> l_starts[K_endog];
  int<lower=1> l_ends[K_endog];

  // pi data
  int<lower=1> N_exog_each[K_exog]; //
  int<lower=1> N_exog_types; //
  real<lower=0> beta_prior[N_exog_types,2];
  int<lower=1> p_starts[K_exog];
  int<lower=1> p_ends[K_exog];
  int<lower=1> p_each[K_exog];
  int<lower=1> p_times[K_exog];

  // ambiguity data
	matrix<lower=0,upper=1>[N_data,N_endog_expand] A;

	// user data
	int<lower=0,upper=2147483647> Y[N_events]; // incorrect dimensionality, only for the tryout

}
parameters {

	vector<lower=0>[N_types-K_endog] gamma;
	real<lower=0,upper=1> pis_base[N_exog_types];

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
	vector[N_endog_expand] lambdas;

	// expansion size (will be changed in the loop for L)
	int expansion = N_endog_each[K_endog];

	matrix[N_data,N_endog_expand] pis = rep_matrix(1, N_data, N_endog_expand);

	vector[N_data] w;


	// fill in L with only lambda draws for last endogenous variable (will be changed in the loop for L)
	lambdas[(N_endog_expand-N_endog_each[K_endog]+1):N_endog_expand] = lambdas_base[l_starts[K_endog]:l_ends[K_endog]];
	// fill in L for the rest of endogenous variables in a BACKWARDS order
	for (i in (K_endog - 1):1) {

		// temproary L fill in
		vector[expansion*N_endog_each[i]] lambdas_temp;

		int m = 1;
		for (j in (N_endog_expand-expansion):N_endog_expand) { // j goes from first to last element position of previous expansion
			for (k in l_starts[i]:l_ends[i]) { // k goes from first to last element position for endog variable i in lambdas_base
				lambdas_temp[m] = lambdas[j] * lambdas_base[k];
				m = m + 1;
			}
		}

		// adjust expansion for the next iteration of the loop
		expansion = expansion * N_endog_each[i];
		// fill in L with temporary L fill in
		lambdas[(N_endog_expand-expansion+1):N_endog_expand] = lambdas_temp;
	}

	for (i in 1:K_exog) {

		vector[N_endog_expand] pis_temp;
		vector[N_exog_each[i]*p_each[i]] pis_temp_each;

		int pos = 1;
		int counter = p_times[i];

		for (j in 1:N_exog_each[i]) {
			pis_temp_each[pos:(pos+p_each[i]-1)] = rep_vector(pis_base[j], p_each[i]);
			pos = pos + p_each[i];
		}

		pis_temp[1:(N_exog_each[i]*p_each[i])] = pis_temp_each;

		while (counter > 1) {
			pis_temp = append_row(pis_temp,pis_temp_each);
			counter = counter - 1;
		}

		pis = pis .* (max_possible_data[,i] * pis_temp' +
								(1 - max_possible_data[,i]) * (1 - pis_temp)');

	}

	w = (A .* pis)' * lambdas;

	// priors
	target += gamma_lpdf(lambdas_base | dirichlet_prior, 1);
	target += beta_lpdf(pis_base | beta_prior[,1], beta_prior[,2]);
	target += multinomial_lpmf(Y | w);

	for (i in 1:K_endog) {
		target += -N_endog_each[i] * log(sum_gammas[i]);
	}
}
