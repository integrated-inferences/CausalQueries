context("Testing summary commands for set_parameter_matrix")

testthat::skip_on_cran()

testthat::test_that(

  desc = "Read P.",

  code = {
    model <- make_model("X -> Y")
    model <- set_parameter_matrix(model, P = get_parameter_matrix(model))
    expect_true(!is.null(model$P))
  }
)


