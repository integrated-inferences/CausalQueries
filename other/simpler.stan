data {

	int<lower=1> N_vars;
	int<lower=1> N_nodal_types;
	int<lower=1> N_types;
	int<lower=1> N_types_each[N_vars];
	int<lower=1> N_data;
	int<lower=1> N_events;
	int<lower=1> N_strategies;

	vector<lower=0>[N_nodal_types] lambdas_prior;
  int<lower=1> l_starts[N_vars];
  int<lower=1> l_ends[N_vars];
  int<lower=1> strategie_starts[N_strategies];
  int<lower=1> strategie_ends[N_strategies];

  vector[N_nodal_types]  P[N_types];
  vector[N_nodal_types] inverted_P[N_types] ;
  matrix<lower=0,upper=1>[N_data, N_types] A;
  matrix<lower=0,upper=1>[N_events,N_data] A_w;
  int<lower=0,upper=2147483647> Y[N_events]; // incorrect dimensionality, only for the tryout

}

parameters {
	vector<lower=0>[N_nodal_types - N_vars] gamma;
}

transformed parameters {
	vector<lower=0>[N_nodal_types] lambdas;
	vector<lower=1>[ N_vars] sum_gammas;
	for (i in 1:N_vars) {
	sum_gammas[i] = 1 + sum(gamma[(l_starts[i] - (i-1)):(l_ends[i] - i)]);
  lambdas[l_starts[i]:l_ends[i]] = append_row(1, gamma[(l_starts[i] - (i-1)):(l_ends[i] - i)]) / sum_gammas[i];
	}
}

model {
	vector[N_data] w;
	vector[N_events] w_full;
	vector[N_types] prob_of_types;
  vector[N_types] P_lambdas[N_nodal_types];



 for (j in 1:N_nodal_types) {
  P_lambdas[j] = P[j] .* lambdas + inverted_P[j];

}

 prob_of_types = rows_dotself(  P_lambdas);





	print("P_lambdas",  P_lambdas);

  w = A * prob_of_types;

	w_full = A_w * w;


	target += gamma_lpdf(lambdas  | lambdas_prior, 1);

	for (i in 1:N_vars) {
		target += -N_types_each[i] * log(sum_gammas[i]);
		target += multinomial_lpmf(Y[strategie_starts[i]:strategie_ends[i]] | w_full[strategie_starts[i]:strategie_ends[i]]);
	}
}
