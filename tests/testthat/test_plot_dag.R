context(desc = "Testing plot_model")

testthat::skip_on_cran()



testthat::test_that(
  desc = "Testing basic functioning",
  code = {
    model <- make_model("X -> M -> Y; X -> Y")
    pdf(file = NULL)
    expect_silent(plot(model))
    dev.off()
  })



testthat::test_that(
  desc = "Testing plot isolate",
  code = {
    model <- make_model("X")
    pdf(file = NULL)
    expect_silent(plot(model))
    dev.off()
  })


testthat::test_that(
  desc = "Testing setting coordinates",
  code = {
    model <- make_model("X -> K -> Y")
    x <- c(1, 2, 3)
    y <- c(1, 1, 1)
    P <- CausalQueries:::plot_model(model,
                                  x_coord = x,
                                  y_coord = y)

    expect_message(
      CausalQueries:::plot_model(model, x_coord = x)
    )

    expect_message(
      CausalQueries:::plot_model(model, y_coord = y)
    )

  })

testthat::test_that(
  desc = "Error messages work",
  code = {
    model <- make_model("X -> K -> Y")

    expect_error(CausalQueries:::plot_model(model = NULL),
                 "Model object must be provided")
    expect_error(CausalQueries:::plot_model(model = c(1, 2, 3)),
                 "Model object must be of type causal_model")
    expect_error(CausalQueries:::plot_model(model,
                      x_coord = c(1, 2, 3),
                      y_coord = c(2, 1)),
                 "x and y coordinates must be of equal length")
    expect_error(CausalQueries:::plot_model(model,
                                          x_coord = c(1, 2),
                                          y_coord = c(2, 1)),
                 "length of coordinates supplied must equal number of nodes")

  })

testthat::test_that(
  desc = "Layout helpers: normalize and confound strength",
  code = {
    # Tiny x-span becomes ~ one layer step
    coords <- data.frame(
      name = c("A", "B", "C", "D", "E"),
      x = c(0, 0, 0, 0, 0.5),
      y = c(5, 4, 3, 2, 1),
      stringsAsFactors = FALSE
    )
    norm <- CausalQueries:::normalize_dag_coords(coords)
    expect_equal(diff(range(norm$x)), 1, tolerance = 1e-8)

    # Pure chain unchanged in x
    chain <- data.frame(name = c("X", "M", "Y"), x = 0, y = 3:1)
    expect_equal(CausalQueries:::normalize_dag_coords(chain)$x, chain$x)

    dag <- data.frame(
      x = c("M", "X"),
      y = c("Y", "M"),
      e = c("<->", "->"),
      weight = 1,
      stringsAsFactors = FALSE
    )
    pos <- data.frame(x = c(0, 0, 0), y = c(2, 3, 1), row.names = c("M", "X", "Y"))
    s_auto <- CausalQueries:::confound_arc_strengths(dag, pos, NULL, 0.3)
    expect_equal(s_auto[2], 0)
    expect_true(abs(s_auto[1]) >= 0.12 && abs(s_auto[1]) <= 0.55)

    s_fixed <- CausalQueries:::confound_arc_strengths(dag, pos, 0.3, 0.2)
    expect_equal(s_fixed[1], 0.3)
    expect_equal(s_fixed[2], 0)
  })

testthat::test_that(
  desc = "shorten_curve_ends pulls endpoints off centres",
  code = {
    df <- data.frame(x = 0, y = 2, xend = 0, yend = 1)
    out <- CausalQueries:::shorten_curve_ends(df, rim = 0.2)
    expect_equal(out$y, 1.8, tolerance = 1e-8)
    expect_equal(out$yend, 1.2, tolerance = 1e-8)
    out2 <- CausalQueries:::shorten_curve_ends(df, rim_start = 0.1, rim_end = 0.3)
    expect_equal(out2$y, 1.9, tolerance = 1e-8)
    expect_equal(out2$yend, 1.3, tolerance = 1e-8)
    expect_null(CausalQueries:::shorten_curve_ends(NULL, 0.2))
  })
