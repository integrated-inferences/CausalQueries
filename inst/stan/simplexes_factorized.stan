/*
 * Factorized / parameters-only CausalQueries Stan model
 *
 * Same Dirichlet simplexes + strategy multinomial as simplexes.stan, but
 * no causal-type matrix P and no type posterior in generated quantities.
 * R prep supplies parmap/map/E from nodal factorization
 * (see memos/factorized_contract.md). Encoding is path-based so confound /
 * missing backends can enrich parmap/map later without a new Stan file.
 */

data {
  int<lower=1> n_params;
  int<lower=1> n_paths;
  int<lower=1> n_param_sets;
  int<lower=1> n_nodes;
  int<lower=1> n_data;
  int<lower=1> n_events;
  int<lower=1> n_strategies;

  array[n_param_sets] int<lower=1> n_param_each;
  vector<lower=0>[n_params] lambdas_prior;

  array[n_param_sets] int<lower=1> l_starts;
  array[n_param_sets] int<lower=1> l_ends;
  array[n_nodes] int<lower=1> node_starts;
  array[n_nodes] int<lower=1> node_ends;
  array[n_strategies] int<lower=1> strategy_starts;
  array[n_strategies] int<lower=1> strategy_ends;

  // Factorized encoding: paths -> data (unconfounded: map = I, paths = data)
  matrix[n_params, n_paths] parmap;
  matrix[n_paths, n_data] map;
  matrix<lower=0, upper=1>[n_events, n_data] E;

  array[n_events] int<lower=0> Y;
}

parameters {
  vector<lower=0>[n_params - n_param_sets] gamma;
}

transformed parameters {
  vector<lower=0, upper=1>[n_params] lambdas;
  vector<lower=1>[n_param_sets] sum_gammas;
  vector[n_param_sets] log_sum_gammas;

  vector<lower=0, upper=1>[n_paths] w_0;
  vector<lower=0, upper=1>[n_data] w;
  vector<lower=0, upper=1>[n_events] w_full;

  for (i in 1:n_param_sets) {
    if (l_starts[i] > l_ends[i]) {
      reject("Invalid parameter set bounds: start > end");
    }

    if (l_starts[i] == l_ends[i]) {
      sum_gammas[i] = 1.0;
      lambdas[l_starts[i]] = 1.0;
    } else {
      sum_gammas[i] = 1.0 + sum(gamma[(l_starts[i] - (i - 1)):(l_ends[i] - i)]);
      vector[l_ends[i] - l_starts[i] + 1] raw_params =
        append_row(1.0, gamma[(l_starts[i] - (i - 1)):(l_ends[i] - i)]);
      lambdas[l_starts[i]:l_ends[i]] = raw_params / sum_gammas[i];
    }
    log_sum_gammas[i] = log(sum_gammas[i]);
  }

  for (i in 1:n_paths) {
    real log_prob = 0.0;
    for (j in 1:n_nodes) {
      real node_prob = sum(lambdas[node_starts[j]:node_ends[j]] .*
                          parmap[node_starts[j]:node_ends[j], i]);
      log_prob += log(node_prob + 1e-10);
    }
    w_0[i] = exp(log_prob);
  }

  w = map' * w_0;
  w_full = E * w;

  if (min(w_full) < 1e-10) {
    reject("Probabilities too close to zero - numerical instability");
  }
}

model {
  for (i in 1:n_param_sets) {
    if (l_starts[i] < l_ends[i]) {
      target += dirichlet_lpdf(lambdas[l_starts[i]:l_ends[i]] |
                              lambdas_prior[l_starts[i]:l_ends[i]]);
      target += -n_param_each[i] * log_sum_gammas[i];
    }
  }

  for (i in 1:n_strategies) {
    vector[strategy_ends[i] - strategy_starts[i] + 1] strategy_probs =
      w_full[strategy_starts[i]:strategy_ends[i]];
    strategy_probs = strategy_probs / sum(strategy_probs);
    target += multinomial_lpmf(
      Y[strategy_starts[i]:strategy_ends[i]] | strategy_probs
    );
  }
}
