
context(desc = "Testing plot_dag")

testthat::skip_on_cran()
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

  desc = "Testing default coordinates with label names",

  code = {
    model <- make_model("X -> K -> Y")
    names <- paste0("test",1:3)
    expect_message(plot(model,lab_names=names))
  }
)

testthat::test_that(

  desc = "Testing setting labels",

  code = {
    model <- make_model("X -> K -> Y")
    x <- c(1,2,3)
    y <- c(1,1,1)
    P <- plot_dag(model,x_coord=x,y_coord=y)
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
    expect_error(plot(model,x_coord = c(1,2),y_coord = c(3,2)))
    expect_error(plot(model, lab_names = paste0("test",1:4)))
    expect_error(plot(model, x_coord = c(1,2,3)))
    expect_error(plot(model, y_coord = c(1,2,3)))
    expect_message(plot(model))
    model <- make_model("X -> K -> Y; X -> Y")
    model <- set_parameter_matrix(model)
    expect_message(plot(model),x_coord=c(1,2,3),y_coord=c(3,2,1))
  }
)


