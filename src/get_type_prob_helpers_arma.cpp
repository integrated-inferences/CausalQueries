#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]

typedef std::vector<double> stdvec;

//' generates one draw from type probability distribution for each type in P
//'
//' @param P parameter_matrix of parameters and causal types
//' @param parameters, priors or posteriors
//' @return draw from type distribution for each type in P
// [[Rcpp::export]]
std::vector<double> get_type_prob_c(arma::mat P, std::vector<double> parameters){
  int ncol = P.n_cols;
  int nrow = P.n_rows;

  for (int j=0; j<ncol; j++) {
    for (int i=0; i<nrow; i++)  {
      P(i,j) = P(i,j) * parameters[i] + 1 - P(i,j);
    }
  }

  std::vector<double> ret;

  for (int j=0; j<ncol; j++) {
    std::vector<double> v_j = arma::conv_to<stdvec>::from(P.col(j));
    ret.push_back(std::accumulate(v_j.begin(), v_j.end(), 1.0, std::multiplies<double>()));
  }
  return ret;
}

//' generates n draws from type probability distribution for each type in P
//'
//' @param params parameters, priors or posteriors
//' @param P parameter_matrix of parameters and causal types
//' @return draws from type distribution for each type in P
// [[Rcpp::export]]
arma::mat get_type_prob_multiple_c(arma::mat params, arma::mat P){

  int ncol = P.n_cols;
  int nrow = params.n_rows;

  arma::mat ret(ncol,nrow);

  for (int i=0; i<nrow; i++){
    ret.col(i) = arma::conv_to<arma::colvec>::from(get_type_prob_c(P,arma::conv_to<std::vector<double>>::from(params.row(i))));
  }

  return ret;
}
