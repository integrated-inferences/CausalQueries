




context("Testing map_query_to_causal_type")

testthat::skip_on_cran()
testthat::test_that(

	desc = "print statement",

	code = {
		model <- make_model('X -> M -> Y; X->Y')
		query <- '(Y[X=1] > Y[X=0]) & (M[X=0]==1)'
		x <- map_query_to_causal_type(model, query)
		out <- capture.output(print(x))
		expect_equal(length(out), 17)

		# print odd number of causal types
		model <- make_model('X -> Y')
		query <- '(Y[X=1] > Y[X=0]) & (X == 0)'
		x <- map_query_to_causal_type(model, query)
		out <- capture.output(print(x))
		expect_equal(out[[9]], " Number of causal types that meet condition(s) =  1")

	}
)

