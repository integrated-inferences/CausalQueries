
context("Query model, query distribution")

testthat::skip_on_cran()
testthat::test_that(

	desc = "Proper error messages.",

	code = {

	  model <- make_model("X -> Y") |>
	    update_model(data.frame(X = 0, Y = 0), refresh = 0)

	  q <- model |>
	    query_distribution(query = "Y[X=1] - Y[X=0]",
	                       using = "parameters",
	                       parameters = 1:6)
	  expect_true(q > 0.0555 & q < 0.0556)

	  expect_message(query_model(model,
	                             query = "X==1",
	                             given = "X==2"))

	  expect_error(query_distribution(list(model, model),
	                                  query = "X==1",
	                                  given = "X==2"))

	  expect_error(query_distribution(model,
	                                  query = "X==1",
	                                  using = 7))

	  expect_error(query_distribution(model,
	                                  query = "X==1",
	                                  case_level = 0:1))

	  expect_error(query_distribution(model,
	                                  parameters = list(1:6, 1:6),
	                                  query = "X==1"))
	  expect_error(query_model(model,
	                           parameters = list(1:6, 1:6),
	                           query = "X==1"))
	  expect_error(query_model(model,
	                           parameters = 1:6,
	                           query = "X==1"))

	  expect_error(query_model(model,  given = 7))

	  expect_error(query_model(model))
	  expect_error(query_model(model, query = 1, queries = 2))

	  q <- query_model(model,
	                   query = "Y[X=1] - Y[X=0]",
	                   case_level = FALSE,
	                   using = "priors")

	  expect_true(is.data.frame(q))
	  expect_true(q$cred.low < -.5 & q$cred.low > -0.7)
	  expect_true(q$cred.high < .7 & q$cred.high > .5)

	  q <- query_model(model,
	                   query = "Y[X=1] - Y[X=0]",
	                   using = "parameters",
	                   parameters = list(c(.5, .5, 0, 0, 1, 0)))
	  expect_true(is.data.frame(q))

	  q <- query_model(
	    model,
	    query = "Y[X=1] - Y[X=0]",
	    using = c("priors", "parameters"),
	    parameters = list(c(.5, .5, 0, 0, 1, 0))
	  )
	  expect_true(round(q[2, 5]) == 1)

	  q <- query_model(
	    model,
	    query = list(ATE = "Y[X=1] - Y[X=0]",
	                 Share_positive = "Y[X=1] > Y[X=0]"),
	    using = c("parameters", "priors", "posteriors"),
	    expand_grid = TRUE,
	    stats = NULL
	  )
	  expect_true(is.data.frame(q))

		}
)



testthat::test_that(

  desc = "query model  behavior.",

  code = {

    # Use saved type_distribution
    model <- make_model("X -> Y") |>
      update_model(data.frame(X=0, Y=0), refresh = 0)

    q <-query_model(
      model,
      query = "Y[X=1] - Y[X=0]",
      using = "posteriors")

    q2 <-query_model(
      model,
      query = "Y[X=1] - Y[X=0]",
      using = "posteriors",
      cred = 99)

    q3 <-query_distribution(
      model,
      query = list("Y[X=1] - Y[X=0]", "Y[X=1] - Y[X=0]"),
      given = c(TRUE, "X==1"),
      using = "parameters")

    q4 <-query_distribution(
      model,
      query = list(a = "Y[X=1] - Y[X=0]", b = "Y[X=1] - Y[X=0]"),
      given = c(TRUE, "X==1"),
      using = "parameters")

    q5 <-query_model(
      model,
      query = "Y[X=1] > Y[X=0]",
      given = "X==1",
      using = "parameters",
      case_level = TRUE)

    expect_true(all(colnames(q3) == c("Y[X=1] - Y[X=0]", "Y[X=1] - Y[X=0] | X==1")))
    expect_true(all(colnames(q4) == c("a", "b")))

    expect_true(is.data.frame(q))
    expect_true(q$cred.low > -0.55 & q$cred.low < -0.45)
    expect_true(q$cred.high > 0.65 & q$cred.high < 0.75)

    expect_true(q$cred.low > q2$cred.low)
    expect_true(q$cred.high < q2$cred.high)
  })
