data {

	int<lower=1> K;
  int<lower=1> K_each[K];
	int<lower=1> N;
	vector<lower=0>[N] dirichlet_prior;
  int<lower=1> starts[K];
  int<lower=1> ends[K];

   int<lower=0,upper=2147483647> Y[N]; // incorrect dimensionality, only for the tryout

}
parameters {

	vector<lower=0>[N-K] gamma;

}
transformed parameters {

	vector<lower=0>[N] w;
	vector<lower=1>[K] sum_gammas;
	for (i in 1:K) {
		sum_gammas[i] = 1 + sum(gamma[(starts[i] - (i-1)):(ends[i] - i)]);
		w[starts[i]:ends[i]] = append_row(1,gamma[(starts[i] - (i-1)):(ends[i] - i)]) / sum_gammas[i];
	}

}
model {

	target += gamma_lpdf(w | dirichlet_prior, 1);

	for (i in 1:K) {
		target += -K_each[i] * log(sum_gammas[i]);
		target += multinomial_lpmf(Y[starts[i]:ends[i]] | w[starts[i]:ends[i]]);
	}
}
