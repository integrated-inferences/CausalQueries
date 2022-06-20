context(desc = "Testing plot_dag")

testthat::skip_on_cran()

testthat::test_that(

  desc = "Testing plot.dag",

  code = {
    model <- make_model("X -> M -> Y; X -> Y")
    pdf(file = NULL)
    expect_silent(plot(model))
    dev.off()
  }
)


testthat::test_that(

  desc = "Testing coordinates",

  code = {
    model <- make_model("X -> K -> Y")
    x <- c(1,2,3)
    y <- c(1,1,1)
    P <- plot(model,x_coord=x,y_coord=y)
    dat <- data.frame(name=model$nodes,x=x,y=y)
    expect_false(!any(P$data[match(P$data$name,model$nodes),c("name","x","y")]==dat))
  }
)


testthat::test_that(

  desc = "Testing setting labels",

  code = {
    model <- make_model("X -> K -> Y")
    x <- c(1,2,3)
    y <- c(1,1,1)
    P <- CausalQueries:::plot_dag(model,x_coord=x,y_coord=y)
    dat <- data.frame(name=model$nodes,x=x,y=y)
    expect_false(!any(P$data[match(P$data$name,model$nodes),c('name','x','y')]==dat))
    expect_message(plot(model,x_coord=x))
    expect_message(plot(model,y_coord=y))

  }
)

testthat::test_that(

  desc = "Test stop arguments",

  code = {
    expect_error(CausalQueries:::plot_dag(model=NULL))
    expect_error(CausalQueries:::plot_dag(model=c(1,2,3)))
    expect_error(plot(model,x_coord = c(1,2,3), y_coord = c(2,1)))
    model <- make_model("X -> K -> Y")
  }
)





