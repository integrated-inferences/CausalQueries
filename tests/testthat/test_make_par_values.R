
context("Test make_par_values")

testthat::test_that(
  desc = "Check non existent column and parameter name errors",
  code = {
    model <- CausalQueries::make_model("X -> Y")

    expect_error(CausalQueries:::make_par_values(
      model = model,
      x = 0.5,
      alter_at = "a == 'X.0'"
    ))

    expect_error(CausalQueries:::make_par_values(
      model = model,
      x = 0.5,
      param_names = "abc"
    ))
  })



testthat::test_that(
  des = "Check distribution specification",
  code = {
    model <- CausalQueries::make_model("X -> Y")

    expect_error(CausalQueries:::make_par_values(
      model = model,
      distribution = "abc"
    ))
  }
)

testthat::test_that(
  des = "check number of parameters and normalization errors",
  code = {
    model <- CausalQueries::make_model("X -> Y")

    # too many values specified
    testthat::expect_error(CausalQueries:::make_par_values(
      model = model,
      node = "Y",
      nodal_type = "00",
      x = c(1, 2)
    ))

    # non numeric values specified
    testthat::expect_error(CausalQueries:::make_par_values(
      model = model,
      node = "Y",
      nodal_type = "00",
      x = "a"
    ))

    # no parameter matched
    testthat::expect_message(
      params <- CausalQueries:::make_par_values(
        model = model,
        node = "Y",
        nodal_type = "0000",
        x = 2
      )
    )

    testthat::expect_equal(params, rep(1,6))

    # normalization warning
    testthat::expect_warning(CausalQueries:::make_par_values(
      model = model,
      node = "Y",
      x = 2,
      normalize = TRUE
    ))
  }
)


testthat::test_that(
  desc = "Check ambigious parameter order warnings",
  code = {
    model <- CausalQueries::make_model("X -> M -> Y")
    expect_warning(CausalQueries:::make_par_values(
      model = model,
      nodal_type = "00",
      x = c(1, 2)
    ))

    model <- CausalQueries::make_model("X -> M -> Y; X <-> Y")
    warnings <-
      testthat::capture_warnings(CausalQueries:::make_par_values(
        model = model,
        node = "Y",
        nodal_type = "00",
        x = c(1, 2)
      ))

    testthat::expect_equal(
      warnings[1],
      paste(
        "A specified condition matches multiple parameters. In these",
        "cases it is unclear which parameter value should be assigned to",
        "which parameter. Assignment thus defaults to the order in which",
        "parameters appear in 'parameters_df'. We advise checking that",
        "parameter assignment was carried out as you intended. "
      )
    )

    testthat::expect_equal(
      warnings[2],
      paste(
        "You are altering parameters on confounded nodes.",
        "Alterations will be applied across all 'param_sets'.",
        "If this is not the alteration behavior you intended,",
        "try specifying the 'param_set' or 'given' option to more",
        "clearly indicate parameters whose values you wish to alter."
      )
    )

  }
)


testthat::test_that(
  desc = "Check output",
  code = {
    model <- CausalQueries::make_model("X -> Y")
    # unambiguous single value case
    out <-
      CausalQueries:::make_par_values(
        model = model,
        x = 2,
        node = "Y"
      )
    expect_equal(out, c(1,1,2,2,2,2))

    # alter_at
    out <-
      CausalQueries:::make_par_values(
        model = model,
        x = c(0.5, 0.25),
        alter_at = "node == 'X' & nodal_type %in% c('0','1')")
    expect_equal(out, c(0.5, 0.25, 1, 1, 1, 1))

    # alter_at with normalization
    out <-
      CausalQueries:::make_par_values(
        model = model,
        x = c(0.5, 0.25),
        alter_at = "node == 'X' & nodal_type %in% c('0','1')",
        normalize = TRUE
      )
    expect_equal(round(out, 2),
                 c(0.67, 0.33, 1.00, 1.00, 1.00, 1.00))

    # node + nodal_type
    out <-
      CausalQueries:::make_par_values(
        model = model,
        x = c(0.5, 0.25),
        node = "X",
        nodal_type = c("0", "1")
      )
    expect_equal(out, c(0.5, 0.25, 1, 1, 1, 1))

    # param_names
    out <-
      CausalQueries:::make_par_values(
        model = model,
        x = c(0.5, 0.25),
        param_names = c("X.0", "X.1")
      )
    expect_equal(out, c(0.5, 0.25, 1, 1, 1, 1))

    # with confounding + specifying param_set
    model <-
      CausalQueries::make_model("X -> Y; X <-> Y")
    out <-
      suppressWarnings(
        CausalQueries:::make_par_values(
          mode = model,
          x = c(0.5, 0.25),
          node = "Y",
          nodal_type = c("00", "01"),
          given = "X.0",
          param_set = "Y.X.0"
        )
      )
    expect_equal(out, c(1, 1, 0.5, 1, 0.25, 1, 1, 1, 1, 1))

    # with confounding using statement + param_set
    out <-
      suppressWarnings(
        CausalQueries:::make_par_values(
          model = model,
          x = c(0.5, 0.25),
          statement = "Y[X=1] > Y[X=0]",
          param_set = c("Y.X.0", "Y.X.1")
        )
      )
    expect_equal(out, c(1, 1, 1, 1, 0.5, 1, 1, 1, 0.25, 1))
  })


testthat::test_that(
  desc = "test make_par_values_stops",
  code = {
    model <- CausalQueries::make_model("X -> Y")

    # can't specify alter_at + node
    expect_error(
      CausalQueires:::make_par_values_stops(
        model = model,
        x = 0.5,
        alter_at = "param_names == 'X.0'",
        node = "X"
      )
    )

    # can't specify param_names + node
    expect_error(
      CausalQueries:::make_par_values_stops(
        model = model,
        x = 0.5,
        param_names = "X.0",
        node = "X"
      )
    )

    # can't specify nodal_type + statement
    expect_error(
      CausalQueries:::make_par_values_stops(
        model = model,
        x = 0.5,
        nodal_type = "00",
        statement = "Y[X = 1] > Y[X = 0]"
      )
    )

    # can't specify node with statement
    expect_error(
      CausalQueries:::make_par_values_stops(
        model = model,
        x = 0.5,
        node = "Y",
        statement = "Y[X=1] > Y[X=0]"
      )
    )

    # can't specify values + distribution
    expect_error(
      CausalQueries:::make_par_values_stops(
        model = model,
        x = 0.5,
        node = "Y",
        distribution = "uniform"
      )
    )

    # can't specify multiple statments
    expect_error(
      CausalQueries:::make_par_values_stops(
        model = model,
        x = 0.5,
        statement = c("Y[X=1] > Y[X=0]", "Y[X=1] > Y[X=0]")
      )
    )

    # can't specify nodal_type + label
    expect_error(
      CausalQueries:::make_par_values_stops(
        model = model,
        x = 0.5,
        nodal_type = "00",
        label = "00"
      )
    )

    # label is deprecated
    expect_warning(
      CausalQueries:::make_par_values_stops(
        model = model,
        x = 0.5,
        label = "00"
      )
    )

    # alter must be character
    expect_error(
      CausalQueries:::make_par_values_stops(
        model = model,
        alter = 1,
        x = 0.5,
        nodal_type = "00"
      )
    )

    # specify only one alter
    expect_error(
      CausalQueries:::make_par_values_stops(
        model = model,
        alter = c("priors","priors"),
        x = 0.5,
        nodal_type = "00"
      )
    )

    # alter must be priors or param_values
    expect_error(
      CausalQueries:::make_par_values_stops(
        model = model,
        alter = c("posteriors"),
        x = 0.5,
        nodal_type = "00"
      )
    )

    # provide either values or distribution
    expect_error(
      CausalQueries:::make_par_values_stops(
        model = model,
        nodal_type = "00"
      )
    )

    # values must be numeric
    expect_error(
      CausalQueries:::make_par_values_stops(
        model = model,
        nodal_type = "00",
        x = c("a","b")
      )
    )

    # values must be non-negative
    expect_error(
      CausalQueries:::make_par_values_stops(
        model = model,
        nodal_type = "00",
        x = c(-1,-2)
      )
    )

    # join by must be character
    expect_error(
      CausalQueries:::make_par_values_stops(
        model = model,
        nodal_type = "00",
        x = 1,
        join_by = 1
      )
    )

    # join by must be | or &
    expect_error(
      CausalQueries:::make_par_values_stops(
        model = model,
        nodal_type = "00",
        x = 1,
        join_by = "a"
      )
    )

    # only specify one join_by
    expect_error(
      CausalQueries:::make_par_values_stops(
        model = model,
        nodal_type = "00",
        x = 1,
        join_by = c("|","&")
      )
    )

  }
)


testthat::test_that(
  desc = "construct_commands_alter_at",
  code = {
    # alter_at must be character
    expect_error(
      CausalQueries:::construct_commands_alter_at(1)
    )

    # specify only cols in parameters_df in alter_at
    expect_error(
      CausalQueries:::construct_commands_alter_at("abc == 1")
    )
  }
)

testthat::test_that(
  desc = "construct_commands_param_names",
  code = {
    # param_names must be character
    expect_error(
      CausalQueries:::construct_commands_param_names(1)
    )

    # specify only param_names in parameters_df
    model <- CausalQueries::make_model("X -> Y")
    expect_error(
      CausalQueries:::construct_commands_param_names(
        param_names = "X.2",
        model_param_names = model$parameters_df$param_names
      )
    )
  }
)

testthat::test_that(
  desc = "construct_commands_other_args",
  code = {
    # args must be character
    expect_error(
      CausalQueries:::construct_commands_other_args(
        node = 1,
        nodal_type = 1,
        param_set = 1,
        given = 1,
        statement = 1
      )
    )
  }
)

