





context("Testing get_event_prob")

testthat::skip_on_cran()
testthat::test_that(desc = "Testing warnings",

                    code = {
                      model <- make_model("X -> Y")
                      expect_error(
                        get_event_prob(model = model, parameters = rnorm(6)),
                        "Negative arguments for parameters not allowed"
                      )
                    })


testthat::test_that(desc = "Testing hack",

                    code = {
                      model <- make_model("X -> Y") |>
                        set_restrictions(c("(X[]==1)", "(Y[X=.]==1)"))
                      expect_true(row.names(get_event_prob(model = model)) ==
                                    colnames(get_ambiguities_matrix(model)))
                    })


testthat::test_that(desc = "Testing given",

                    code = {
                      p <- make_model('X ->  Y') |>
                        get_event_prob(given = "X!=Y") |>
                        data.frame() |>
                        pull(event_probs)
                      expect_true(all(p == c(0, .5, .5, 0)))
                    })


