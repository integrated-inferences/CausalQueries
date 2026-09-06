# Test suite path control (factorized default vs legacy)
#
# CQ_TEST_LEGACY:
#   FALSE (default) — factorized path (package interactive default)
#   TRUE — force legacy causal-type path for the whole suite
# CQ_TEST_LEGACY_OBJECTS:
#   TRUE (default during transition) — run tests that require attached
#     causal_types / type_posterior / P after make_model/update
#   FALSE — skip those via skip_if_legacy_objects()
#
# Override examples (PowerShell):
#   $env:CQ_TEST_LEGACY="TRUE"; ...
#   $env:CQ_TEST_LEGACY_OBJECTS="FALSE"; ...

cq_test_legacy <- Sys.getenv("CQ_TEST_LEGACY", "FALSE")
options(CausalQueries.legacy = as.logical(cq_test_legacy))

cq_test_legacy_objects <- Sys.getenv("CQ_TEST_LEGACY_OBJECTS", "TRUE")
options(CausalQueries.test_legacy_objects = as.logical(cq_test_legacy_objects))

#' Skip when legacy-object attachment tests are turned off.
#' @keywords internal
skip_if_legacy_objects <- function() {
  if (!isTRUE(getOption("CausalQueries.test_legacy_objects", FALSE))) {
    testthat::skip(
      "legacy-object tests off (CQ_TEST_LEGACY_OBJECTS / CausalQueries.test_legacy_objects)"
    )
  }
}

#' Run an expression with CausalQueries.legacy forced TRUE.
#' @keywords internal
with_legacy_true <- function(code) {
  old <- getOption("CausalQueries.legacy")
  on.exit(options(CausalQueries.legacy = old), add = TRUE)
  options(CausalQueries.legacy = TRUE)
  force(code)
}
