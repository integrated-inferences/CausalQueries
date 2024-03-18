context("Testing set_confound")

testthat::skip_on_cran()

testthat::test_that(

	desc = "Test alias is working ",

	code = {
		model <-
		  make_model('X -> Y') |> set_confound(list('Y <-> X'))
		models <-
		  make_model('X -> Y') |> set_confound(list('X <-> Y'))

		expect_identical(model, models)
	}
)


testthat::test_that(

  desc = "Adding confounds to model statememt",

  code = {
    model <-
      make_model('X -> Y -> W') |>
      set_confound(list('Y <-> X', 'X <-> W'))

    expect_identical(model$statement, "X -> Y -> W; Y <-> X; W <-> X")

    expect_message(
      make_model('X -> Y -> W') |> set_confound(),
      "No confound provided"
    )

  }
)


testthat::test_that(

  desc = "Test for confound errors",

  code = {
    expect_error(
      make_model('X -> Y -> W; X <-> Y') |>
      set_confound(list('Y <-> X', 'X <-> W'))
    )

    expect_error(
      make_model('X -> Y -> W') |>
        set_confound(list('Y <-> X; X <-> W'))
    )
  }
)



testthat::test_that(

  desc = "Degrees of freedom from confounding",

  code = {

    dof <-
      function(model) {
        model$parameters_df |>
          group_by(param_set) |>
          summarize(n  = n() -1) |>
          pull(n) |>
          sum()
      }

    # 2 * 4 - 1  = 1 + 3 + 3 = 7
    expect_identical(
      dof(make_model('X -> Y; X <-> Y')), 7
    )

    # 1 + 3 + 4 * 15 = 64: one path model
    expect_identical(
      dof(make_model('X -> M -> Y <- X; M <-> Y')), 64
    )

    # 1 + 2*3 + 4* 15 = 67: two path model
    expect_identical(
      dof(make_model('X -> M -> Y <- X; X <-> M; M <-> Y')), 67
    )

    # 1 + 2*3 + 8* 15 = 127 (full distribution on thetas)
    expect_identical(
      dof(make_model('X -> M -> Y <- X; X <-> M; M <-> Y; X <-> Y')), 127
    )

    # 1 +  1 + 4*15 = 62
    expect_identical(
      dof(make_model('A -> C <- B; A <-> C; B <-> C')), 62
    )

    # 1 +  2 + 15 = 18
    expect_identical(
      dof(make_model('A -> C <- B; A <-> B')), 18
    )

    # 1 +  6 + 6  = 15
    expect_identical(
      dof(make_model('A <- C -> B; A <-> C; B <-> C')), 13
    )

    # # non binary (4 * 4) model: 3 dof for X,  4^4 -1 = 255 for Y: 258
    # expect_identical(
    #   make_model("Y2 <- X1 -> Y1; Y2 <- X2 ->Y1; X1 <-> X2; Y1 <-> Y2") |>
    #     dof(),
    #   258
    # )

  }
)




testthat::test_that(

  desc = "parmap after confounding",

  # example where parmap implies two 'paths' and has dimensionality of
  # (number of parameters) x (number of data types * 2)
  code = {
    model <- make_model('X -> M -> Y <- X; M <-> Y')
    expect_equal(
      model |> get_parmap() |> dim(),
      c(model$parameters_df |> nrow(), 2 * 2^length(model$nodes)))

  }
)


testthat::test_that(

  desc = "Removing exsiting distributions works",

  code = {
    model <-
      make_model('X -> Y -> W') |> set_prior_distribution(n_draws = 5)

    expect_equal(
      dim(model$prior_distribution),
      c(5, 10)
    )

    expect_null(
      set_confound(model, list('Y <-> X', 'X <-> W'))$prior_distribution
    )


    model <-
      make_model('X -> Y -> W') |>
      update_model(data.frame(X = 0, Y = 0),
                   refresh = 0, chains = 1, iter = 10, warmup = 5) |>
      suppressWarnings()

    expect_equal(dim(model$posterior_distribution), c(5, 10))

    expect_null(
      set_confound(model, list('Y <-> X', 'X <-> W'))$prior_distribution
    )

    expect_null(
      set_confound(model, list('Y <-> X', 'X <-> W'))$stan_objects
    )

  }
)
