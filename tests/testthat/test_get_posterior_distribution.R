





context("Testing get_posterior_distribution")

testthat::test_that(desc = "Without",

                    code = {
                      model <- make_model("X -> Y")
                      expect_message(
                        get_posterior_distribution(model = model),
                        "The model does not contain a posterior distribution. A posterior distribution can be provided to the model using `update_model`"
                      )
                    })

testthat::skip_on_cran()
testthat::test_that(desc = "With",

                    code = {
                      model <- make_model("X -> Y") |> update_model()
                      expect_true(
                        is.data.frame(get_posterior_distribution(model = model))
                      )
                    })

