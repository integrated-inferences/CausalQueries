




context("Testing get_query_types")

testthat::test_that(

	desc = "Check output works fine",
testthat::skip_on_cran()
	code = {
		model <- make_model('X -> M -> Y; X->Y')
		query <- '(Y[X=0] > Y[X=1])'
		expect_error(get_query_types(model, query, map=LETTERS[1]))
		expect_identical(map_query_to_causal_type(model, query), get_query_types(model, query, map="causal_type"))
		expect_identical(map_query_to_nodal_type(model, query), get_query_types(model, query, map="nodal_type"))
		expect_identical(map_query_to_causal_type(model, query), get_query_types(model, query))

	}
)

