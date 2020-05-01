

.runThisTest <- Sys.getenv("RunAllRcppTests") == "yes"

if (.runThisTest) {

context(desc = "Testing map_query_to_nodal_type")

testthat::test_that(
	desc = "print and class of summaries",
	code = {
		model <- make_model('X -> M -> Y; X->Y')
		query <- '(Y[X=0]>Y[X=1])'
		x <- map_query_to_nodal_type(model, query)
		w <- summary(x)
		expect_true(class(x) == "nodal_types")
		expect_true("summary.nodal_types" %in% class(w))
    expect_output(print(x), "Nodal types satisfying query's condition")
    b <- capture.output(print(x))
    test_output <- " query :  (Y[X=0]>Y[X=1]) "
    expect_false(any(grepl(test_output, b, fixed = TRUE)))
    query <- '(Y[X=0]>Y[X=1])'
		model <- make_model('X -> Y')
		x <- map_query_to_nodal_type(model, query)
		b <- capture.output(print(x))
		expect_true(any(grepl(test_output, b, fixed = TRUE)))
	}
)
}
