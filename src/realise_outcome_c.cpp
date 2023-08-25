#include <Rcpp.h>

//fast string to int conversion
int fast_stoi( const char * str ) {
  int val = 0;
  while( *str ) {
    val = val*10 + (*str++ - '0');
  }
  return val;
}

// [[Rcpp::export]]
std::vector<std::vector<std::string>> realise_outcomes_c(std::vector<std::vector<std::string>>& real,
                                                         const std::vector<std::vector<int>>& parents_list,
                                                         const std::vector<int>& endogenous_vars,
                                                         const int& n_types) {
  for(int i = 0; i < endogenous_vars.size(); ++i) {
    int endog_var = endogenous_vars[i];

    for(int j = 0; j < n_types; ++j) {
      std::string type = real[endog_var][j];
      int pos = 0;

      for(int k = 0; k < parents_list[endog_var].size(); ++k){
        const char * parent_k  = real[parents_list[endog_var][k]][j].c_str();
        int parent_val = fast_stoi(parent_k);
        pos += (1 << k) * parent_val;
      }

      real[endog_var][j] = type[pos];
    }
  }
  return real;
}
