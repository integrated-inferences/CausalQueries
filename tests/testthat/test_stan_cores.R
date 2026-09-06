testthat::test_that("stan_cores is CRAN-safe and sets mc.cores", {
  old_env <- Sys.getenv("_R_CHECK_LIMIT_CORES_", unset = NA)
  old_cores <- getOption("mc.cores")
  on.exit({
    if (is.na(old_env)) {
      Sys.unsetenv("_R_CHECK_LIMIT_CORES_")
    } else {
      Sys.setenv(`_R_CHECK_LIMIT_CORES_` = old_env)
    }
    options(mc.cores = old_cores)
  }, add = TRUE)

  Sys.setenv(`_R_CHECK_LIMIT_CORES_` = "TRUE")
  expect_equal(CausalQueries:::stan_cores(), 2L)

  Sys.unsetenv("_R_CHECK_LIMIT_CORES_")
  n <- CausalQueries:::enable_stan_parallel(quiet = TRUE)
  expect_true(is.integer(n) && length(n) == 1L && n >= 1L)
  expect_equal(getOption("mc.cores"), n)
})
