data {

int<lower=1> n_params;
int<lower=1> n_types;
int<lower=1> n_param_sets;
int<lower=1> n_param_each[n_param_sets];
int<lower=1> n_data;
int<lower=1> n_events;
int<lower=1> n_strategies;

vector<lower=0>[n_params] lambdas_prior;
int<lower=1> l_starts[n_param_sets];
int<lower=1> l_ends[n_param_sets];
int<lower=1> strategy_starts[n_strategies];
int<lower=1> strategy_ends[n_strategies];

vector[n_types] P[n_params] ;
vector[n_types] not_P[n_params] ;
matrix<lower=0,upper=1>[n_types, n_data] A;
matrix<lower=0,upper=1>[n_events,n_data] E;
int<lower=0> Y[n_events];

}

parameters {
vector<lower=0>[n_params - n_param_sets] gamma;
}

transformed parameters {
vector<lower=0>[n_params] lambdas;
vector<lower=1>[n_param_sets] sum_gammas;
for (i in 1:n_param_sets) {

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
vector[n_params] P_lambdas[n_types];

for (i in 1:n_types) {
  for (j in 1:n_params) {
    P_lambdas[i, j] = P[j, i] .* lambdas[j] + not_P[j, i];
  }
  prob_of_types[i] = prod(P_lambdas[i]);
}

w = A' * prob_of_types;
w_full = E * w;

target += gamma_lpdf(lambdas  | lambdas_prior, 1);

for (i in 1:n_param_sets) {
  target += -n_param_each[i] * log(sum_gammas[i]);
 }

for (i in 1:n_strategies) {
  target += multinomial_lpmf(
  Y[strategy_starts[i]:strategy_ends[i]] | w_full[strategy_starts[i]:strategy_ends[i]]);
 }

}

