
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
	  expect_true(round(q[2, 6]) == 1)

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

    # Use saved type_posterior
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

    expect_true(all(colnames(q3) == c("Y[X=1] - Y[X=0]", "Y[X=1] - Y[X=0] :|: X==1")))
    expect_true(all(colnames(q4) == c("a", "b")))

    expect_true(is.data.frame(q))
    expect_true(q$cred.low > -0.55 & q$cred.low < -0.45)
    expect_true(q$cred.high > 0.65 & q$cred.high < 0.75)

    expect_true(q$cred.low > q2$cred.low)
    expect_true(q$cred.high < q2$cred.high)

    expect_message(
      query_model(
      model,
      query = "Y[X=1] > Y[X=0]",
      given = "X==1",
      case_level = c(TRUE, FALSE),
      labels = "A"), "labels have been provided but are of incorrect length: 2 labels required")

    expect_message(
      query_model(
        model,
        query = "Y[X=1] > Y[X=0]",
        given = "X==1",
        case_level = c(TRUE, FALSE),
        labels = c("A", "A")))

    expect_equivalent(
      query_model(
        model,
        query = "Y[X=1] > Y[X=0]",
        given = "X==1",
        case_level = c(TRUE, FALSE),
        labels = c("A", "B")) |> pull(label),
      c("A", "B"))

  })





testthat::test_that(

  desc = "Expand grid behavior.",

  code = {


models <- list(
 M1 = make_model("X -> Y"),
 M2 = make_model("X -> Y") |>
   set_restrictions("Y[X=1] < Y[X=0]")
 )

n1 <- query_model(
  models,
  query = list(ATE = "Y[X=1] - Y[X=0]",
               Share_positive = "Y[X=1] > Y[X=0]"),
  given = c(TRUE,  "Y==1 & X==1"),
  using = c("parameters", "priors"),
  expand_grid = FALSE) |> nrow()

n1b <- query_model(
  models,
  query = list(ATE = "Y[X=1] - Y[X=0]", Share_positive = "Y[X=1] > Y[X=0]"),
  given = c(TRUE,  "Y==1 & X==1"),
  using = c("parameters", "priors")) |> nrow()

# Expands over query and given argument
n2 <- query_model(
  models,
  query = list(ATE = "Y[X=1] - Y[X=0]",
               Share_positive = "Y[X=1] > Y[X=0]"),
  given = c(TRUE,  "Y==1 & X==1"),
  using = c("parameters", "priors"),
  expand_grid = TRUE) |> nrow()



# query and given arguments coupled
n3 <- query_model(
  models,
  query = list(ATE = "Y[X=1] - Y[X=0]",
               Share_positive = "Y[X=1] > Y[X=0] :|: Y==1 & X==1"),
  using = c("parameters", "priors"),
  expand_grid = TRUE
) |> nrow()

# query and given arguments coupled: figures out to expand grid
n4 <- query_model(
  models,
  query = list(
    ATE = "Y[X=1] - Y[X=0]",
    Share_positive = "Y[X=1] > Y[X=0] :|: Y==1 & X==1",
    P = "Y[X=1] ==1"
  ),

  using = c("parameters", "priors")
) |> nrow()

expect_true(n1 == 4)
expect_true(n1b == 4)
expect_true(n2 == 16)
expect_true(n3 == 8)
expect_true(n4 == 12)

# expand_grid = FALSE but uneven lengths
expect_error(query_model(
  models,
  query = list(
    ATE = "Y[X=1] - Y[X=0]",
    Share_positive = "Y[X=1] > Y[X=0] :|: Y==1 & X==1",
    P = "Y[X=1] ==1"
  ),
  using = c("parameters", "priors"),
  expand_grid = FALSE
))



  }
)




testthat::test_that(

  desc = "No operator confusion.",

  code = {

    q1 <- query_model(
      make_model("X -> Y"),
      query = "Y[X=1] > Y[X=0] | Y[X=1] < Y[X=0] :|: Y[X=1] >= Y[X=0]",
      )

    expect_true(q1$mean == 1/3)

    q2 <- query_model(
      make_model("X -> Y"),
      query = "(Y[X=1] > Y[X=0]) & (Y[X=1] < Y[X=0]) :|: Y[X=1] >= Y[X=0]",
    )

    expect_true(q2$mean == 0)


    # Provide queries once
    expect_error(
      query_model(
      make_model("X -> Y"),
      query = "(Y[X=1] > Y[X=0]) & (Y[X=1] < Y[X=0]) :|: Y[X=1] >= Y[X=0]",
      given = "Y==1"
    )
    )

    # Provide queries once
    expect_error(
      query_distribution(
        make_model("X -> Y"),
        query = "(Y[X=1] > Y[X=0]) & (Y[X=1] < Y[X=0]) :|: Y[X=1] >= Y[X=0]",
        given = "Y==1"
      )
      )

  }
)
