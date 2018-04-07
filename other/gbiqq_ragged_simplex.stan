data {

	int<lower=1> K; // number of variables
	int<lower=1> K_endog; // number of endogenous variables
	int<lower=1> K_exog; // number of exogenous variables
  int<lower=1> N_endog_each[K_endog]; // number of types for each endogenous variable
	int<lower=1> N_types; // sum(N_endog_each)
	int<lower=1> N_events; // number of possible events (e.g. X0K0Y1, X0K0, etc.)
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

	// strategies data
	int<lower=1> K_strategies; // number of strategy types (e.g. XKY, XK, XY, KY, X, K, Y)
	int<lower=1> w_starts[K_strategies];
	int<lower=1> w_ends[K_strategies];
	matrix<lower=0,upper=1>[N_events,N_data] A_w;

	// user data
	int<lower=0,upper=2147483647> Y[N_events]; // incorrect dimensionality, only for the tryout

	// debugging
	// vector<lower=0,upper=1>[N_types] lambdas_base;
}
parameters {

	vector<lower=0>[N_types-K_endog] gamma;
	real<lower=0,upper=1> pis_base[N_exog_types];

}
transformed parameters {

	// this is a workaround for vector of estimated simpleces for dirichlet distribution
	vector<lower=0,upper=1>[N_types] lambdas_base;
	vector<lower=1>[K_endog] sum_gammas;
	for (i in 1:K_endog) {
		sum_gammas[i] = 1 + sum(gamma[(l_starts[i] - (i-1)):(l_ends[i] - i)]);
		lambdas_base[l_starts[i]:l_ends[i]] =
			append_row(1,gamma[(l_starts[i] - (i-1)):(l_ends[i] - i)]) / sum_gammas[i];
	}

}
model {

	// expanded vector of lambdas (L)
	vector[N_endog_expand] lambdas;

	// expansion size (will be changed in the loop for L)
	int expansion = N_endog_each[K_endog];
	int endog_i = K_endog - 1; // helps implementing backwars order

	// pis matrix placeholder for multiplication by pi matrices for each exogenous variable
	matrix[N_data,N_endog_expand] pis = rep_matrix(1, N_data, N_endog_expand);

	// weights placeholder
	vector[N_data] w;
	vector[N_events] w_full;

	// LAMBDAS STUFF

	// fill in L with only lambda draws for last endogenous variable (will be changed in the loop for L)
	lambdas[(N_endog_expand - expansion + 1):N_endog_expand] =
		lambdas_base[l_starts[K_endog]:l_ends[K_endog]];

	// fill in L for the rest of endogenous variables in a BACKWARDS order
	// while-loop is used to implement backwards order, since for-loop doesnt take descending
	// indeces
	while (endog_i > 0) {

		// temproary L fill in
		vector[expansion*N_endog_each[endog_i]] lambdas_temp;
		int m = 1;

		for (j in (N_endog_expand - expansion + 1):N_endog_expand) {
			// j goes from first to last element position of previous expansion
			for (k in l_starts[endog_i]:l_ends[endog_i]) {
				// k goes from first to last element position for endog variable i in lambdas_base
				lambdas_temp[m] = lambdas[j] * lambdas_base[k];
				m = m + 1;
			}
		}

		// adjust expansion for the next iteration of the loop
		expansion = expansion * N_endog_each[endog_i];
		endog_i = endog_i - 1;
		// fill in L with temporary L fill in
		lambdas[(N_endog_expand - expansion + 1):N_endog_expand] = lambdas_temp;
	}

	// PIS STUFF

	for (i in 1:K_exog) {

		// pis_temp is pi matrix for i's exogenous variable
		// pis_temp_each is a workaround the fact that STAN cannot create vector by repeating each element of it
		vector[N_endog_expand] pis_temp;
		vector[N_exog_each[i]*p_each[i]] pis_temp_each;

		// position and repeat times counters
		int pos = 1;
		int counter = p_times[i];

		// take each element of pis_base and repeat it p_each times and store results in pis_temp_each
		for (j in 1:N_exog_each[i]) {
			pis_temp_each[pos:(pos+p_each[i]-1)] = rep_vector(pis_base[j], p_each[i]);
			pos = pos + p_each[i];
		}

		// put the result in pis_temp
		pis_temp[1:(N_exog_each[i]*p_each[i])] = pis_temp_each;

		// create final pis_temp by appending pis_temp_each p_times times
		while (counter > 1) {
			pis_temp = append_row(pis_temp,pis_temp_each);
			counter = counter - 1;
		}

		// take existing pis matrix (initially matrix of 1's) and dot multiply it by correct values
		// (pi if for row where corresponding exogenous variable is 1, and (1 - pi) otherwise);
		// the resulting matrix of pis is of dimensions nrow(A), ncol(A), or N_data and N_endog_expand
		// respectively
		pis = pis .* (max_possible_data[,i] * pis_temp' +
								(1 - max_possible_data[,i]) * (1 - pis_temp)');

	}

	// WHERE MAGIC HAPPENS:
	// A - N_data x N_endog_expand ambiguity matrix
	// pis -- N_data x N_endog_expand matrix of combined product of pis for all exogenous variables
	// lambdas -- column vector of length N_endog_expand of products of all combinations of lambdas
	// A_w -- ambiguity matrix transfering weights for each of max data weights into data events

	w = (A .* pis) * lambdas;
	w_full = A_w * w;

	// debugging
	// print("w: ", w);

	// LIKELIWOOD

	// multinomial prior for Y
	// This is currently causing problems for models with large numbers of w's
	// and seems to be an issue with underflowing 0's
	// see http://discourse.mc-stan.org/t/dirichlet-log-probabilities-is-not-a-valid-simplex/1805/2
  for (i in 1:K_strategies) {
  	target += multinomial_lpmf(Y[w_starts[i]:w_ends[i]] | w_full[w_starts[i]:w_ends[i]]);
  }

	// PRIORS

	// gamma prior for lambdas which ensures lambdas ~ dirichlet(dirichlet_prior)
	// this is a workaround necessary to deal with vector of vectors of simpleces
	target += gamma_lpdf(lambdas_base | dirichlet_prior, 1);

	// correction required for identification of gamma implementation for dirichlet priors on lambda
	for (i in 1:K_endog) {
		target += -N_endog_each[i] * log(sum_gammas[i]);
	}

	// gamma prior
	// target += uniform_lpdf(gamma | 0, 10);

	// beta prior for pis
	target += beta_lpdf(pis_base | beta_prior[,1], beta_prior[,2]);

}
