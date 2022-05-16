
context("Test make_par_values")

testthat::skip_on_cran()
testthat::test_that(

	desc = "Check disallow redundant arguments",

	code = {
	  model <- CausalQueries::make_model("X -> Y")
	  expect_error(CausalQueries:::make_par_values(model = model, x = 0.5, alter_at = "param_names == 'X.0'", node = "X"))
	  expect_error(CausalQueries:::make_par_values(model = model, x = 0.5, param_names = "X.0", node = "X"))
	  expect_error(CausalQueries:::make_par_values(model = model, x = 0.5, nodal_type = "00", statement = "Y[X = 1] > Y[X = 0]"))
	  expect_error(CausalQueries:::make_par_values(model = model, x = 0.5, node = "Y", statement = "Y[X=1] > Y[X=0]"))
	  expect_error(CausalQueries:::make_par_values(model = model, x = 0.5, node = "Y", distribution = "uniform"))
	}
)


testthat::skip_on_cran()
testthat::test_that(

  desc = "Check non existent column and parameter name errors",

  code = {
    model <- CausalQueries::make_model("X -> Y")
    expect_error(CausalQueries:::make_par_values(model = model, x = 0.5, alter_at = "a == 'X.0'"))
    expect_error(CausalQueries:::make_par_values(model = model, x = 0.5, param_names = "abc"))
  }

)


testthat::skip_on_cran()
testthat::test_that(

  desc = "Check output",

  code = {
    model <- CausalQueries::make_model("X -> Y")
    out <- CausalQueries:::make_par_values(model = model, x = c(0.5,0.25), alter_at = "node == 'X' & nodal_type %in% c('0','1')")
    expect_equal(out, c(0.5,0.25,1,1,1,1))
    out <- CausalQueries:::make_par_values(model = model, x = c(0.5,0.25), alter_at = "node == 'X' & nodal_type %in% c('0','1')", normalize = TRUE)
    expect_equal(round(out,2), c(0.67,0.33,1.00,1.00,1.00,1.00))
    out <- CausalQueries:::make_par_values(model = model, x = c(0.5,0.25), node = "X", nodal_type = c("0","1"))
    expect_equal(out, c(0.5,0.25,1,1,1,1))
    out <- CausalQueries:::make_par_values(model = model, x = c(0.5,0.25), param_names = c("X.0","X.1"))
    expect_equal(out, c(0.5,0.25,1,1,1,1))

    model <- CausalQueries::make_model("X -> Y; X <-> Y")
    out <- CausalQueries:::make_par_values(mode = model, x = c(0.5,0.25), node = "Y", nodal_type = c("00","01"), given = "X.0", param_set = "Y.X.0")
    expect_equal(out, c(1,1,0.5,1,0.25,1,1,1,1,1))
    out <- CausalQueries:::make_par_values(model = model, x = c(0.5,0.25), statement = "Y[X=1] > Y[X=0]")
    expect_equal(out, c(1,1,1,1,0.5,1,1,1,0.25,1))
  }

)






