# Test equivalence of two Stan computation approaches
library(CausalQueries)

# Setup test data
model <- make_model('X -> M -> Y; M <-> Y')
parmap <- CausalQueries:::make_parmap(model)
lambdas <- make_parameters(model, param_type = "prior_draw")

# Get node information (simulating what Stan would have)
nodes <- unique(model$parameters_df$node)
node_starts <- sapply(nodes, function(n) min(which(model$parameters_df$node == n)))
node_ends <- sapply(nodes, function(n) max(which(model$parameters_df$node == n)))
n_nodes <- length(nodes)
n_paths <- ncol(parmap)

cat("Test setup:\n")
cat("n_params:", nrow(parmap), "\n")
cat("n_paths:", ncol(parmap), "\n") 
cat("n_nodes:", n_nodes, "\n")
cat("nodes:", paste(nodes, collapse = ", "), "\n")
cat("node_starts:", paste(node_starts, collapse = ", "), "\n")
cat("node_ends:", paste(node_ends, collapse = ", "), "\n\n")

# Approach 1: Original CRAN-style (with full parlam matrix)
approach1 <- function(lambdas, parmap, node_starts, node_ends) {
  n_params <- nrow(parmap)
  n_paths <- ncol(parmap)
  n_nodes <- length(node_starts)
  
  # Create full parlam matrix
  parlam <- matrix(lambdas, nrow = n_params, ncol = n_paths) * parmap
  
  # Sum over nodes for each path
  parlam2 <- matrix(0, nrow = n_nodes, ncol = n_paths)
  for (i in 1:n_nodes) {
    a <- node_starts[i]
    b <- node_ends[i]
    parlam2[i, ] <- colSums(parlam[a:b, , drop = FALSE])
  }
  
  # Compute path probabilities
  w_0 <- numeric(n_paths)
  for (i in 1:n_paths) {
    w_0[i] <- exp(sum(log(parlam2[, i])))
  }
  
  return(list(parlam2 = parlam2, w_0 = w_0))
}

# Approach 2: Optimized (direct computation)
approach2 <- function(lambdas, parmap, node_starts, node_ends) {
  n_nodes <- length(node_starts)
  n_paths <- ncol(parmap)
  
  # Direct computation for each node
  parlam2 <- matrix(0, nrow = n_nodes, ncol = n_paths)
  for (j in 1:n_nodes) {
    a <- node_starts[j]
    b <- node_ends[j]
    # This is equivalent to: lambdas[a:b]' * parmap[a:b, ]
    parlam2[j, ] <- colSums(matrix(lambdas[a:b], nrow = length(a:b), ncol = n_paths) * parmap[a:b, , drop = FALSE])
  }
  
  # Vectorized computation
  w_0_local <- exp(colSums(log(parlam2)))
  
  return(list(parlam2 = parlam2, w_0 = w_0_local))
}

# Run both approaches
result1 <- approach1(lambdas, parmap, node_starts, node_ends)
result2 <- approach2(lambdas, parmap, node_starts, node_ends)

# Compare results
cat("=== COMPARISON RESULTS ===\n")

cat("\nparlam2 matrices identical:", all.equal(result1$parlam2, result2$parlam2), "\n")
if (!all.equal(result1$parlam2, result2$parlam2)) {
  cat("Max difference in parlam2:", max(abs(result1$parlam2 - result2$parlam2)), "\n")
}

cat("\nw_0 vectors identical:", all.equal(result1$w_0, result2$w_0), "\n")
if (!all.equal(result1$w_0, result2$w_0)) {
  cat("Max difference in w_0:", max(abs(result1$w_0 - result2$w_0)), "\n")
}

# Show first few values for inspection
cat("\n=== SAMPLE VALUES ===\n")
cat("Approach 1 w_0[1:5]:", paste(round(result1$w_0[1:5], 8), collapse = ", "), "\n")
cat("Approach 2 w_0[1:5]:", paste(round(result2$w_0[1:5], 8), collapse = ", "), "\n")

cat("\nApproach 1 parlam2[1:2, 1:3]:\n")
print(round(result1$parlam2[1:2, 1:3], 6))

cat("\nApproach 2 parlam2[1:2, 1:3]:\n")
print(round(result2$parlam2[1:2, 1:3], 6))

# Test with multiple random draws
cat("\n=== TESTING MULTIPLE RANDOM DRAWS ===\n")
all_match <- TRUE
for (i in 1:5) {
  test_lambdas <- make_parameters(model, param_type = "prior_draw")
  r1 <- approach1(test_lambdas, parmap, node_starts, node_ends)
  r2 <- approach2(test_lambdas, parmap, node_starts, node_ends)
  
  match_parlam2 <- all.equal(r1$parlam2, r2$parlam2)
  match_w0 <- all.equal(r1$w_0, r2$w_0)
  
  if (!match_parlam2 || !match_w0) {
    all_match <- FALSE
    cat("Draw", i, "- MISMATCH!\n")
    if (!match_parlam2) cat("  parlam2 diff:", max(abs(r1$parlam2 - r2$parlam2)), "\n")
    if (!match_w0) cat("  w_0 diff:", max(abs(r1$w_0 - r2$w_0)), "\n")
  } else {
    cat("Draw", i, "- MATCH ✓\n")
  }
}

cat("\n=== FINAL CONCLUSION ===\n")
if (all_match) {
  cat("✅ BOTH APPROACHES ARE MATHEMATICALLY EQUIVALENT!\n")
} else {
  cat("❌ APPROACHES PRODUCE DIFFERENT RESULTS!\n")
}
