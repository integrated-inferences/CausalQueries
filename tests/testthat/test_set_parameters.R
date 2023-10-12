




context("Testing set_parameters")

testthat::skip_on_cran()
testthat::test_that(

	desc = "Main arguments",

	code = {
		model <- make_model('X -> Y')

		expect_error(make_parameters(model, param_type = 'posterior_draw'))
		expect_error(make_parameters(model, param_type = 'posterior_mean'))

		model <- update_model(model, data.frame(X=1, Y=1), refresh = 0)
		a <- make_parameters(model, parameters = c(.25, .75, 1.25,.25, .25, .25))
		expect_true(length(a)==6)

		a <-make_parameters(model)
		expect_true(length(a)==6)

		a <-make_parameters(model, param_type = 'flat')
		expect_true(length(a)==6)

		a <- make_parameters(model, param_type = 'prior_draw')
		expect_true(length(a)==6)

		a <- make_parameters(model, param_type = 'prior_mean')
		expect_true(length(a)==6)

		a <- make_parameters(model, param_type = 'posterior_draw')
		expect_true(length(a)==6)

		a <- make_parameters(model, param_type = 'posterior_mean')
		expect_true(length(a)==6)

#		a <- make_parameters(model, statement = "Y[X=1] > Y[X=0]", parameters = .5)
#		expect_true(a[5]==.5)

#		a <- make_parameters(model, statement = "Y[X=1] > Y[X=0]", parameters = .5, normalize = FALSE)
#		expect_true(a[5]==.4)

		a <- get_parameters(model, param_type = 'posterior_mean')
		expect_true(length(a)==6)

		a <- get_parameters(model)
		expect_true(length(a)==6)

		model <- make_model("X -> Y")

		a <- make_parameters(model = model, parameters = c(0.5,0.25), alter_at = "node == 'X' & nodal_type %in% c('0','1')", normalize = TRUE)
		expect_equal(unname(round(a,2)), c(0.67,0.33,0.25,0.25,0.25,0.25))

		a <- make_parameters(model = model, parameters = c(0.5,0.25), node = "X", nodal_type = c("0","1"))
		expect_equal(unname(round(a,2)), c(0.67,0.33,0.25,0.25,0.25,0.25))

		a <- make_parameters(model = model, parameters = 0.9, param_names = "X.0")
		expect_equal(unname(a), c(0.9,0.1,0.25,0.25,0.25,0.25))

		expect_error(make_parameters(model, param_type = 'flatbread'))

		expect_true(class(set_parameters(model)) == "causal_model")

		a <- make_model('X -> Y') |>
  		set_confound("X <-> Y")

	  a <- suppressWarnings(set_parameters(a,statement = 'Y[X=1]>Y[X=0]', param_set = c("Y.X.0","Y.X.1"), parameters = c(.2, .8)))
  	a <- get_parameters(a)
		expect_equal(sum(0.2 == a), 1)

		}
)



