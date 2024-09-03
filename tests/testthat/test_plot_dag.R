context(desc = "Testing plot_model")

testthat::skip_on_cran()

testthat::test_that(
  desc = "Testing plot.dag",
  code = {
    model <- make_model("X -> M -> Y; X -> Y")
    pdf(file = NULL)
    expect_silent(plot(model))
    dev.off()
  })


testthat::test_that(
  desc = "Testing coordinates",
  code = {
    model <- make_model("X -> K -> Y")
    x <- c(1, 2, 3)
    y <- c(1, 1, 1)
    P <- plot(model, x_coord = x, y_coord = y)
    dat <- data.frame(name = model$nodes,
                      x = x,
                      y = y)
    expect_false(!any(P$data[match(P$data$name, model$nodes),
                             c("name", "x", "y")] == dat))
  })


testthat::test_that(
  desc = "Testing setting labels",
  code = {
    model <- make_model("X -> K -> Y")
    x <- c(1, 2, 3)
    y <- c(1, 1, 1)
    P <- CausalQueries:::plot_model(model,
                                  x_coord = x,
                                  y_coord = y)

    expect_message(
      CausalQueries:::plot_model(model, x_coord = x)
    )

    expect_message(
      CausalQueries:::plot_model(model, y_coord = y)
    )

  })

testthat::test_that(
  desc = "Error messages work",
  code = {
    model <- make_model("X -> K -> Y")

    expect_error(CausalQueries:::plot_model(model = NULL),
                 "Model object must be provided")
    expect_error(CausalQueries:::plot_model(model = c(1, 2, 3)),
                 "Model object must be of type causal_model")
    expect_error(CausalQueries:::plot_model(model,
                      x_coord = c(1, 2, 3),
                      y_coord = c(2, 1)),
                 "x and y coordinates must be of equal length")
    expect_error(CausalQueries:::plot_model(model,
                                          x_coord = c(1, 2),
                                          y_coord = c(2, 1)),
                 "length of coordinates supplied must equal number of nodes")

  })




