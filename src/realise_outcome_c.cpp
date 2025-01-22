#include <Rcpp.h>

// fast string to int conversion
int str_to_int( const char * str ) {
  int val = 0;
  while( *str ) {
    val = val*10 + (*str++ - '0');
  }
  return val;
}


// explanations assume vector indexing from 0 (as in C++) instead of 1 (as in R)

// generating realized outcomes can be achieved by traversing the DAG from
// exogenous (or intervened upon) nodes in generational order recording the
// value of a child node implied by the realizations of that child node's parents.
//
// nodal types encode a child node's response to its parents' realizations.
// Getting the realization value implied by parent realizations boils down to
// figuring out the index in the nodal_type that corresponds to a set of parent
// realizations. By way of example: in an X -> Y; Z -> Y model the value at
// index 0 in Y's nodal types corresponds to the value Y takes on under
// the parent realization X = 0; Z = 0. See CausalQueries:::interpret_type()
// for details.
//
// nodal types are constructed as the Cartesian product of parent realizations.
// By way of example in an X -> Y; Z -> Y model generating the Cartesian product
// of X = {0,1} and Z = {0,1} yields:
// X Y
// 0 0 --> [*]***
// 1 0 --> *[*]**
// 0 1 --> **[*]*
// 1 1 --> ***[*]
// each row in the above matrix corresponds to an index in the nodal types of Y.
// Finding the position of the realization value of Y in a nodal type given
// parent realizations is equivalent to finding the corresponding row number
// in the Cartesian product matrix.
// By definition of the Cartesian product, the number of consecutive 0 or 1
// elements in a given column is 2^(column_index) when indexing columns from 0.
// Given a set of parent realizations R indexed from 0, the corresponding row
// number in the Cartesian product matrix indexed from 0 can thus be computed
// via: (2^i * R[i]) summed for each value of i from i = 0 to i = |R| - 1.
// Powers of two with integer exponents can be efficiently computed via
// bit-shifting in C++ (1 << i).

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
        int parent_val = str_to_int(parent_k);
        pos += (1 << k) * parent_val;
      }

      real[endog_var][j] = type[pos];
    }
  }
  return real;
}
