
context(desc = "Testing plot_dag")

testthat::skip_on_cran()
testthat::test_that(

  desc = "Testing plot.dag",

  code = {
    model <- make_model("X -> M -> Y; X -> Y")
    pdf(file = NULL)
    expect_silent(plot(model))
    model <- make_model("X -> M -> Y")  %>%
      set_confound(confound = list(M = "Y[M=1]==1"))
    model$confounds_df <- NULL
    expect_that(plot_dag(model), shows_message())
  }
)

testthat::test_that(

  desc = "Testing warning",

  code = {
    model <- make_model('X -> K -> Y; X -> Y')
    model <- set_parameter_matrix(model)
    expect_message(plot_dag(model))
  }
)

testthat::test_that(

  desc = "Testing coordinates",

  code = {
    model <- make_model('X -> K -> Y')
    x <- c(1,2,3)
    y <- c(1,1,1)
    P <- plot_dag(model,x=x,y=y)
    dat <- data.frame(name=model$nodes,x=x,y=y)
    expect_false(!any(P$data[match(P$data$name,model$nodes),c('name','x','y')]==dat))
  }
)

testthat::test_that(

  desc = "Testing obscure",

  code = {
    model <- make_model('X -> K -> Y')
    obs <- "X->K"
    P <- plot_dag(model,obscure=obs)
    expect_true(P$data %>% dplyr::filter(grepl("X",name) & grepl("K",to)) %>% .$direction %>%
                  is.na())
  }
)
