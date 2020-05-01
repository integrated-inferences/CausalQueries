library(testthat)

library(CausalQueries)

test_check("CausalQueries")

if (length(strsplit(packageDescription("CausalQueries")$Version, "\\.")[[1]]) > 3) {
  Sys.setenv("RunAllRcppTests"="yes")
}
