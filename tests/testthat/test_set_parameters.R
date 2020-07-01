




context("Testing set_parameters")

testthat::test_that(

	desc = "Main arguments",
testthat::skip_on_cran()
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

		a <- make_parameters(model,  statement = "Y[X=1] > Y[X=0]", parameters = .5)
		expect_true(a[5]==.5)

		a <- make_parameters(model, statement = "Y[X=1] > Y[X=0]", parameters = .5, normalize = FALSE)
		expect_true(a[5]==.4)

		a <- get_parameters(model, param_type = 'posterior_mean')
		expect_true(length(a)==6)

		a <- get_parameters(model)
		expect_true(length(a)==6)

		expect_error(make_parameters(model, param_type = 'flatbread'))

		expect_true(class(set_parameters(model)) == "causal_model")

		out <- make_model('X -> Y') %>%
  		set_confound(list(X = 'Y[X=1]>Y[X=0]'))  %>%
  		set_parameters(confound = list(X='Y[X=1]>Y[X=0]', X='Y[X=1]<=Y[X=0]'),
                 parameters = list(c(.2, .8), c(.8, .2))) %>%
  		get_parameters
		expect_equal(sum(0.2 == out), 2)
		}
)



