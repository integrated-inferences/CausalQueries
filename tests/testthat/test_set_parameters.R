context("Testing set_parameters")

testthat::test_that(
	desc = "Main arguments",
	code = {
		model <- make_model('X -> Y')

		expect_error(make_parameters(model, param_type = 'posterior_draw'))
		expect_error(make_parameters(model, param_type = 'posterior_mean'))

		model <- update_model(model, data.frame(X=1, Y=1))
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

		a <- make_parameters(model, param_type = 'define', statement = "Y[X=1] > Y[X=0]", alphas = .5)
		expect_true(a[5]==.4)

		a <- make_parameters(model, statement = "Y[X=1] > Y[X=0]", alphas = .5)
		expect_true(a[5]==.4)

		a <- get_parameters(model, param_type = 'posterior_mean')
		expect_true(length(a)==6)

		a <- get_parameters(model)
		expect_true(length(a)==6)

		expect_error(make_parameters(model, param_type = 'flatbread'))

		expect_true(class(set_parameters(model)) == "causal_model")


		}
)

