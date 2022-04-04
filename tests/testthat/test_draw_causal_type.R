

context("draw_causal_type")

testthat::skip_on_cran()
testthat::test_that(

  desc = "Test draw_causal_type output.",

  code = {
    model <- make_model("X -> Y")

    types <- draw_causal_type(model)

    expect_true(all(dim(types) == c(6,9)))
    expect_true(all(types[[1]] == c("X.0","X.1","Y.00","Y.10","Y.01","Y.11")))

  }
)
