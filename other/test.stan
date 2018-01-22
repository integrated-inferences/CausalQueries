
data {
  int<lower=0> n_strategies; // number of strategies that can be pursued to collect data
  int<lower=0> n_data_realizations; // number of all possible realizations of data, given all strategies
  int<lower=0> n_fundamental_data_realizations; // number of possible realizations of data, given data sought on all nodes 



  int<lower=0> X[2];      // number of cases with X only data
  int<lower=0> K[2];      // number of cases with K only data
  int<lower=0> Y[2];      // number of cases with Y only data
  int<lower=0> XY[4];     // number of XY cases: order: 00, 01, 10, 11
  int<lower=0> XK[4];     // number of XY cases:: 00, 01, 10, 11
  int<lower=0> KY[4];     // summary data, providing number of XY: order: 00, 01, 10, 11
  int<lower=0> XKY[8];    // summary data, providing number of XKY: order 000, 001, 010,...

  vector[16]    Y_u_prior;    // initial hyperparameters for the Dirichlet distribution; provided as data
  vector[4]     K_u_prior;    // initial hyperparameters for the Dirichlet distribution; provided as data. Note independent priors on Y and K.
  matrix[16,4]  pi_alpha;     // initial hyperparameters for the beta distributions for pi_a, pi_b  etc
  matrix[16,4]  pi_beta ;     // initial hyperparameters for the beta distributions for pi_a, pi_b  etc
  matrix[16,4]  types ;       // type_mapping
  }

parameters {
  simplex[16] lambda_Y;                          // Parameters of interest: share of Y causal types in population
  simplex[4]  lambda_K;                          // Parameters of interest: share of K causal types in population
  matrix<lower=0,upper=1>[16, 4] pi;              // pi for each primitive value of Y, K
  }

transformed parameters {
// Parameters determine the multinomial event probabilities

  simplex[n_data_realizations] w;             
  simplex[n_fundamental_data_realizations] w_fundamental;


  w_fundamental = lambda %*% t(A*pi_new);

  

 }




model {
  lambda_Y ~ dirichlet(Y_u_prior);               // Priors for causal types
  lambda_K ~ dirichlet(K_u_prior);               // Priors for causal types

  // 16 * 4 pi values need to be defined --
  for (r in 1:16) {
    for (c in 1:4)  {
    pi[r,c] ~ beta(pi_alpha[r,c], pi_beta[r,c]);
    }}               // Priors for ps

  for (i in 1:n_strategies){
    D[starts[i]:ends[i]]  ~  multinomial(w[starts[i]:ends[i]])  ;   // Likelihood:
  }
 }

generated quantities{

}