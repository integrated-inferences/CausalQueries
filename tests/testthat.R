library(testthat)

library(CausalQueries)

# Parallel Stan chains across the suite (rstan respects options(mc.cores)).
# Skip when already set (e.g. user / CI). Keep at least 1.
if (is.null(getOption("mc.cores"))) {
  n_cores <- suppressWarnings(parallel::detectCores())
  if (is.na(n_cores) || n_cores < 1L) {
    n_cores <- 1L
  }
  options(mc.cores = n_cores)
}

test_check("CausalQueries")

if (length(strsplit(packageDescription("CausalQueries")$Version, "\\.")[[1]]) > 3) {
  Sys.setenv("RunAllRcppTests"="yes")
}
