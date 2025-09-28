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

  // log-sum-exp for small vectors
  real log_sum_exp_stable(vector x) {
    real max_x = max(x);
    return max_x + log(sum(exp(x - max_x)));
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

  // === PROBABILITY COMPUTATION ===
  vector<lower=0, upper=1>[n_data] w;

  // Convert gamma to simplex lambdas
  for (i in 1:n_param_sets) {
    if (l_starts[i] == l_ends[i]) {
      // Single parameter case
      lambdas[l_starts[i]] = 1.0;
    } else {
      // Multiple parameters - use stick-breaking
      real sum_gammas_i = 1.0 + sum(gamma[(l_starts[i] - (i - 1)):(l_ends[i] - i)]);

      // Simplex construction
      vector[l_ends[i] - l_starts[i] + 1] raw_params =
        append_row(1.0, gamma[(l_starts[i] - (i - 1)):(l_ends[i] - i)]);

      lambdas[l_starts[i]:l_ends[i]] = raw_params / sum_gammas_i;
    }
  }

  // === PROBABILITY COMPUTATION ===
  // Compute path probabilities into a local w_0, then use BLAS GEMV for w = map' * w_0
  {
    vector[n_paths] w_0_local;
    for (i in 1:n_paths) {
      real log_prob = 0.0;

      for (j in 1:n_nodes) {
        // Sum probabilities over parameters for this node
        real node_prob = sum(lambdas[node_starts[j]:node_ends[j]] .*
                            parmap[node_starts[j]:node_ends[j], i]);

        // Add small constant to avoid log(0)
        log_prob += log(node_prob + 1e-10);
      }
      w_0_local[i] = exp(log_prob);
    }
    w = map' * w_0_local;
  }
}

model {
  // === DIRICHLET PRIORS ===
  for (i in 1:n_param_sets) {
    if (l_starts[i] < l_ends[i]) {
      target += dirichlet_lpdf(lambdas[l_starts[i]:l_ends[i]] |
                              lambdas_prior[l_starts[i]:l_ends[i]]);
      // Recompute normalization locally to avoid saving log_sum_gammas
      {
        real sum_gammas_i = 1.0 + sum(gamma[(l_starts[i] - (i - 1)):(l_ends[i] - i)]);
        target += -n_param_each[i] * log(sum_gammas_i);
      }
    }
  }

  // === MULTINOMIAL LIKELIHOOD ===
  // Compute event probabilities once here
  vector[n_events] p = E * w;
  for (i in 1:n_strategies) {
    vector[strategy_ends[i] - strategy_starts[i] + 1] strategy_probs =
      p[strategy_starts[i]:strategy_ends[i]];

    // Normalize probabilities
    strategy_probs = strategy_probs / sum(strategy_probs);

    target += multinomial_lpmf(
      Y[strategy_starts[i]:strategy_ends[i]] | strategy_probs
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
