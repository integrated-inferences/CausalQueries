data {
	int<lower=2> K;
	int<lower=0,upper=2147483647> x[K];
}
parameters {
	vector<lower=0>[K-1] gamma;
}
transformed parameters {
	real sum_gamma = 1 + sum(gamma);
	vector[K] pi = append_row(1,gamma) / sum_gamma;
}
model {
	target += -K *log(sum_gamma);
	target += dirichlet_lpdf(pi | rep_vector(1,K));
	x ~ multinomial(pi);
	// target += multinomial_lpmf(x | pi);
}
