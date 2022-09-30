#include <Rcpp.h>
using namespace Rcpp;

//' generates realized outcomes for all causal types by sequentially calculating endogenous nodes.
//'
//' @param d a data.frame of causal types passed from within realise_outcomes
//' @param endogenous_nodes a character vector with names of endogenous nodes
//' @param parents_list a list of parents nodes for each node
//' @param nodal_types_collapsed a list of collapsed nodal types
//' @param nodal_types a list of uncolpased nodal types
//' @return a data.frame of realized outcomes for each causal type
// [[Rcpp::export]]

DataFrame realise_outcome_c(DataFrame d,
                            std::vector<std::string> endogenous_nodes,
                            List parents_list,
                            List nodal_types_collapsed,
                            List nodal_types){

  //generate a copy of d (avoids modification of d in the parent environment)
  DataFrame data_realization = clone(d);

  //loop over each endogenous node
  for(int i = 0; i < endogenous_nodes.size(); i++){

    //get name of endogenous node
    std::string var = endogenous_nodes[i];
    //get causal type realizations for endogenous node
    std::vector<std::string> child_type = data_realization[var];
    //get parents of endogenous node
    std::vector<std::string> parents = parents_list[var];
    //get nodal types of endogenous node
    std::vector<std::string> nodal_label = nodal_types_collapsed[var];
    //get uncolapsed nodal types for endogenous node
    DataFrame nodal_type_var = nodal_types[var];
    //generate empty vector for realizations
    std::vector<std::string> J;

    //loop over causal types
    for(int j = 0; j < child_type.size(); j++){
      //get causal type
      std::string type = child_type[j];
      //generate empty vector for parent realization
      std::string parents_val;
      //get parent realization
      for(int k = 0; k < parents.size(); k++){
        std::vector<std::string> parent_val_k = data_realization[parents[k]];
        parents_val.append(parent_val_k[j]);
      }
      //find row position of type
      std::vector<std::string>::iterator row_it = std::find(nodal_label.begin(),nodal_label.end(), type);
      int row = row_it - nodal_label.begin();
      //find realization and add to J
      std::vector<int> outcome = nodal_type_var[parents_val];
      std::string outcome_str = std::to_string(outcome[row]);
      J.push_back(outcome_str);

    }
    //put realized outcomes into data_realization
    data_realization[var] = J;

  }

  return data_realization;

}
