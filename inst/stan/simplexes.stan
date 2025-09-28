/*
 * Causal Inference Model with Simplex Constraints
 *
 * This model implements Bayesian causal inference with:
 * - Dirichlet priors on parameter simplexes
 * - Multinomial likelihood for observed data
 * - Support for confounded and censored data
 */

functions {
  // Column sum function
  row_vector col_sums(matrix X) {
    return rep_row_vector(1, rows(X)) * X;
  }
}

data {
  // === DIMENSIONS ===
  int<lower=1> n_params;           // Number of parameters
  int<lower=1> n_paths;            // Number of causal paths
  int<lower=1> n_types;            // Number of causal types
  int<lower=1> n_param_sets;       // Number of parameter sets
  int<lower=1> n_nodes;            // Number of nodes
  int<lower=1> n_data;             // Number of data types
  int<lower=1> n_events;           // Number of observed events
  int<lower=1> n_strategies;       // Number of strategies
  int<lower=0, upper=1> keep_type_distribution;

  // === PARAMETER SETUP ===
  array[n_param_sets] int<lower=1> n_param_each;
  vector<lower=0>[n_params] lambdas_prior;

  // === INDEXING ARRAYS ===
  array[n_param_sets] int<lower=1> l_starts;
  array[n_param_sets] int<lower=1> l_ends;
  array[n_nodes] int<lower=1> node_starts;
  array[n_nodes] int<lower=1> node_ends;
  array[n_strategies] int<lower=1> strategy_starts;
  array[n_strategies] int<lower=1> strategy_ends;

  // === MODEL MATRICES ===
  matrix[n_params, n_types] P;            // Parameter-type mapping
  matrix[n_params, n_paths] parmap;       // Parameter-path mapping
  matrix[n_paths, n_data] map;            // Path-data mapping
  matrix<lower=0, upper=1>[n_events, n_data] E;  // Event-data mapping

  // === OBSERVED DATA ===
  array[n_events] int<lower=0> Y;         // Observed counts
}

// (Index validation is performed in R before calling Stan)

parameters {
  // Unconstrained parameters (will be transformed to simplex)
  vector<lower=0>[n_params - n_param_sets] gamma;
}

transformed parameters {
  // === SIMPLEX CONSTRAINT HANDLING ===
  vector<lower=0, upper=1>[n_params] lambdas;
  vector<lower=1>[n_param_sets] sum_gammas;
  vector[n_param_sets] log_sum_gammas;

  // === PROBABILITY COMPUTATION ===
  vector<lower=0, upper=1>[n_data] w;

  // Convert unconstrained parameters to simplex probabilities
  for (i in 1:n_param_sets) {
    if (l_starts[i] == l_ends[i]) {
      // Single parameter case
      lambdas[l_starts[i]] = 1.0;
      sum_gammas[i] = 1.0;
    } else {
      // Multiple parameters - construct simplex
      sum_gammas[i] = 1.0 + sum(gamma[(l_starts[i] - (i - 1)):(l_ends[i] - i)]);

      // Simplex construction
      vector[l_ends[i] - l_starts[i] + 1] raw_params =
        append_row(1.0, gamma[(l_starts[i] - (i - 1)):(l_ends[i] - i)]);

      lambdas[l_starts[i]:l_ends[i]] = raw_params / sum_gammas[i];
    }
    log_sum_gammas[i] = log(sum_gammas[i]);
  }

  // === PROBABILITY COMPUTATION ===
  {
    vector[n_paths] w_0_local;
    matrix[n_params, n_paths] parlam;
    matrix[n_nodes, n_paths] parlam2;

    // Map parameters to causal paths
    parlam = rep_matrix(lambdas, n_paths) .* parmap;

    // Sum probabilities over nodes for each path
    for (i in 1:n_nodes) {
      parlam2[i, ] = col_sums(parlam[node_starts[i]:node_ends[i], ]);
    }

    // Calculate path probabilities
    for (i in 1:n_paths) {
      w_0_local[i] = exp(sum(log(parlam2[, i])));
    }

    // Map paths to data types
    w = map' * w_0_local;
  }
}

model {
  // === DIRICHLET PRIORS ===
  for (i in 1:n_param_sets) {
    target += dirichlet_lpdf(lambdas[l_starts[i]:l_ends[i]] |
                            lambdas_prior[l_starts[i]:l_ends[i]]);
    target += -n_param_each[i] * log_sum_gammas[i];
  }

  // === MULTINOMIAL LIKELIHOOD ===
  for (i in 1:n_strategies) {
    target += multinomial_lpmf(
      Y[strategy_starts[i]:strategy_ends[i]] |
      w[strategy_starts[i]:strategy_ends[i]] /
      sum(w[strategy_starts[i]:strategy_ends[i]])
    );
  }
}

generated quantities {
  // Distribution of causal types
  vector[n_types] types;

  if (keep_type_distribution == 1) {
    // Compute causal type probabilities
    for (i in 1:n_types) {
      types[i] = prod(P[, i] .* lambdas + (1 - P[, i]));
    }
  } else {
    types = rep_vector(1.0, n_types);
  }
}
