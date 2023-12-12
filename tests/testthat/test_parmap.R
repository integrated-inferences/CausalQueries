context("Testing parmap")

testthat::skip_on_cran()

testthat::test_that(

  desc = "Make parmap works",

  code = {
    expect_equal(
      dim(make_parmap(model = make_model('X -> M -> Y; M <-> Y'))),
      c(22, 16)
    )

    expect_equal(
      dim(make_parmap(model = make_model('X->Y; X<->Y')) |> attr("map")),
      c(4, 4)
    )
  }
)

testthat::test_that(

  desc = "Get/set parmap works",

  code = {
    expect_equal(
      make_parmap(model = make_model('X -> Y')),
      get_parmap(model = make_model('X -> Y'))
    )

    expect_equal(
      get_parmap(model = set_parmap(make_model('X -> Y'))),
      get_parmap(model = make_model('X -> Y'))
    )
  }
)
