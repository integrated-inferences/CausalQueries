





context("Testing get_posterior_distribution")

testthat::test_that(desc = "Without",

                    code = {
                      model <- make_model("X -> Y")
                      expect_warning(
                        get_posterior_distribution(model = model),
                        "'get_posterior_distribution' is deprecated."
                      )
                    })

