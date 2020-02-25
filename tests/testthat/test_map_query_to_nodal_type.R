
context(desc = "Testing map_query_to_nodal_type")

testthat::test_that(
	desc = "print and class of summaries",
	code = {
		model <- make_model('X -> M -> Y; X->Y')
		query <- '(Y[X=0] > Y[X=1])'
		x <- map_query_to_nodal_type(model, query)
		w <- summary(x)
		expect_true(class(x) == "nodal_types")
		expect_true("summary.nodal_types" %in% class(w))
    expect_output(print(x), "Nodal types satisfying query's condition")
	}
)

