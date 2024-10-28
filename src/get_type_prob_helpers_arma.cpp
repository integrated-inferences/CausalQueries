#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]

// [[Rcpp::export]]
std::vector<double> get_type_prob_c(const arma::mat& P, const std::vector<double>& parameters) {
  int ncol = P.n_cols;
  int nrow = P.n_rows;

  std::vector<double> result(ncol);

  for (int j = 0; j < ncol; j++) {
    double prod = 1.0;
    for (int i = 0; i < nrow; i++) {
      prod *= P(i, j) * parameters[i] + 1 - P(i, j);
    }
    result[j] = prod;
  }
  return result;
}

// [[Rcpp::export]]
arma::mat get_type_prob_multiple_c(arma::mat params, arma::mat P) {
  int ncol = P.n_cols;
  int nrow = params.n_rows;

  arma::mat ret(ncol, nrow);

  for (int i = 0; i < nrow; i++) {
    std::vector<double> result = get_type_prob_c(P, arma::conv_to<std::vector<double>>::from(params.row(i)));
    ret.col(i) = arma::vec(result);
  }

  return ret;
}
