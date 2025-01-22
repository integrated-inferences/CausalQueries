





context(desc = "Testing map_query_to_nodal_type")

testthat::skip_on_cran()
testthat::test_that(

	desc = "correct mapping",

	code = {
	  model <- make_model("X -> Y")
	  map <- map_query_to_nodal_type(model, "Y[X=1] > Y[X=0]")

	  node <- "Y"
	  nodal_types <- c("00","10","01","11")
	  implicated <- c("01")

	  expected_map <- nodal_types %in% implicated
	  names(expected_map) <- nodal_types

	  expect_equal(map$node, node)
	  expect_equal(map$types, expected_map)
	}
)

