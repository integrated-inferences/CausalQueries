context("Testing make_confounders")

testthat::test_that(
	desc = "Check output.",
	code = {
		model <- make_model("X -> Y -> Z") %>%
  		set_confound(list("X <-> Y"))
		out <- get_nodal_joint_probability(model)
		expect_equal(rowSums(out[, c("X0", "X1")]), rep(1, 10))
		expect_true(all(get_nodal_joint_probability(model, parameters = abs(rnorm(14)))[, 3:8]>= 0))
	}
)

testthat::test_that(
	desc = "Check options.",
	code = {
		model <- make_model("X -> Y -> Z") %>%
  		set_confound(list("X <-> Y"))
		out <- get_nodal_joint_probability(model)
		expect_true(all(0.5 == get_nodal_joint_probability(model, generic_parameters = FALSE)[3:10, 3:4]))
		expect_false(all(0.5 == get_nodal_joint_probability(model, generic_parameters = TRUE)[3:10, 3:4]))
	}
)

testthat::test_that(
	desc = "Check messages.",
	code = {
		model <- make_model("X -> Y")
		expect_message(make_confounds_df(model))
	}
)
