#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]

// Product over causal types: for each column j of P,
//   prod_i (P(i,j) * parameters[i] + 1 - P(i,j))
// When P is 0/1 incidence (usual case), this is the product of parameters[i]
// over rows with P(i,j)==1. Skip P==0 rows (multiply by 1).

// [[Rcpp::export]]
std::vector<double> get_type_prob_c(const arma::mat& P,
                                    const std::vector<double>& parameters) {
  const arma::uword ncol = P.n_cols;
  const arma::uword nrow = P.n_rows;

  std::vector<double> result(ncol);

  for (arma::uword j = 0; j < ncol; j++) {
    double prod = 1.0;
    for (arma::uword i = 0; i < nrow; i++) {
      const double pij = P(i, j);
      if (pij == 0.0) {
        continue;
      }
      if (pij == 1.0) {
        prod *= parameters[i];
      } else {
        prod *= pij * parameters[i] + 1.0 - pij;
      }
    }
    result[j] = prod;
  }
  return result;
}

// [[Rcpp::export]]
arma::mat get_type_prob_multiple_c(const arma::mat& params,
                                   const arma::mat& P) {
  const arma::uword ncol = P.n_cols;
  const arma::uword n_draws = params.n_rows;
  const arma::uword n_params = P.n_rows;

  arma::mat ret(ncol, n_draws);

  std::vector<double> parameters(n_params);
  for (arma::uword d = 0; d < n_draws; d++) {
    for (arma::uword i = 0; i < n_params; i++) {
      parameters[i] = params(d, i);
    }
    std::vector<double> result = get_type_prob_c(P, parameters);
    for (arma::uword j = 0; j < ncol; j++) {
      ret(j, d) = result[j];
    }
  }

  return ret;
}
