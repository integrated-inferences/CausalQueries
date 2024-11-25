context(desc = "Testing plot_model_query")

testthat::skip_on_cran()



testthat::test_that(
  desc = "Testing plot query",
  code = {
    model <- make_model("X -> Y")
    q <- query_model(model, c("Y[X=1] - Y[X=0]", "Y[X=1] > Y[X=0]"),
                     given = c("Y==1", TRUE),
                     using = c("priors", "parameters"),
                     expand_grid = TRUE)
    pdf(file = NULL)
    expect_silent(plot(q))
    dev.off()
  })




