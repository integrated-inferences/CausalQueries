data {
	int<lower=1> n_vars;
	int<lower=1> n_nodal_types;
	int<lower=1> n_types;
	int<lower=1> n_types_each[n_vars];
	int<lower=1> n_data;
	int<lower=1> n_events;
	int<lower=1> n_strategies;

	vector<lower=0>[n_nodal_types] lambdas_prior;
	int<lower=1> l_starts[n_vars];
	int<lower=1> l_ends[n_vars];
  int<lower=1> strategy_starts[n_strategies];
  int<lower=1> strategy_ends[n_strategies];

  vector[n_types] P[n_nodal_types] ;
  vector[n_types] inverted_P[n_nodal_types] ;
  matrix<lower=0,upper=1>[n_data, n_types] A;
  matrix<lower=0,upper=1>[n_events,n_data] A_w;
  int<lower=0,upper=2147483647> Y[n_events]; // incorrect dimensionality, only for the tryout

}

parameters {
	vector<lower=0>[n_nodal_types - n_vars] gamma;
}

transformed parameters {
	vector<lower=0>[n_nodal_types] lambdas;
	vector<lower=1>[ n_vars] sum_gammas;
	for (i in 1:n_vars) {

	sum_gammas[i] =
			1 + sum(gamma[(l_starts[i] - (i-1)):(l_ends[i] - i)]);

  lambdas[l_starts[i]:l_ends[i]] =
  		append_row(1, gamma[(l_starts[i] - (i-1)):(l_ends[i] - i)]) / sum_gammas[i];

	}
}

model {
	vector[n_data] w;
	vector[n_events] w_full;
	vector[n_types] prob_of_types;
  vector[n_nodal_types] P_lambdas[n_types];

  for (i in 1:n_types) {
  	for (j in 1:n_nodal_types) {
  		P_lambdas[i, j] = P[j, i] .* lambdas[j] + inverted_P[j, i];
  	}
  	prob_of_types[i] = prod(P_lambdas[i]);
  }

  w = A * prob_of_types;
  w_full = A_w * w;


  target += gamma_lpdf(lambdas  | lambdas_prior, 1);
  for (i in 1:n_vars) {
  	target += -n_types_each[i] * log(sum_gammas[i]);
  }

  for (i in 1:n_strategies) {
  	target += multinomial_lpmf(
  		Y[strategy_starts[i]:strategy_ends[i]] | w_full[strategy_starts[i]:strategy_ends[i]]);
  }
}
