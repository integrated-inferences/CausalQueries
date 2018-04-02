data {
  // Data for likelihood
  int<lower=1> n_events;
  int<lower=0> Y[n_events];

  // Priors
  vector<lower=0>[n_lambdas] lambdas_prior; // alphas for dirichlet distribution
  matrix[n_pis,2] pis_prior; // alphas and betas for beta distribution

  // Likelihood Helpers
  int<lower=1> n_strategies;
  int<lower=1> w_starts[n_strategies];
  int<lower=1> w_ends[n_strategies];
  
  A_w

  // Endogenous Type Helpers
  l_starts
  l_ends

  // Exogenous Type Helpers
  p_starts
  p_ends

}
parameters {
  vector[n_age] a;
  real<lower=0,upper=100> sigma_a;

  lambdas
  pis

}
transformed parameters {
  vector[N] y_hat;
  w_full[n_events];

  

  // Expand fundamental data events into full probabilities of
  // all possible data events
  // w should be a column vector
  w_full = A_w * w; 

}
model { 
  // Likelihood
  for (i in 1:n_strategies) {
    // For each data strategy, fit a likelihood
    Y[w_starts[i]:w_ends[i]]  ~  multinomial(w_full[w_starts[i]:w_ends[i]]);  
  }

  // Priors on endogenous types
  for (i in 1:n_endog) {
    // For each endogenous variable, draw a vector from the dirichlet distribution
    lambdas[l_starts[i]:l_ends[i]] ~ dirichlet(lambdas_prior[l_starts[i]:l_ends[i]]);  
  }

  // Priors on exogenous variables
  for (i in 1:n_exog) {
    // For each exogenous variable, draw a vector from th beta distribution
    pis[p_starts[i]:p_ends[i]] ~ beta(pis_prior[(p_starts[i]:p_ends[i]),1],pis_prior[(p_starts[i]:p_ends[i]),2]);
  }
}













