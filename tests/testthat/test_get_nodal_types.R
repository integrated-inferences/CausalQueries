context("Testing get_nodal_types")

testthat::test_that(
	desc = "Condition for make_nodal_types",
	code = {
		out_true <- gbiqq:::make_nodal_types(make_model("X -> Y"), include_node_names = TRUE)
		expect_true(all(grepl("X", rownames(out_true$X))))
		out_false <- gbiqq:::make_nodal_types(make_model("X -> Y"), include_node_names = FALSE)
		expect_false(any(grepl("X", rownames(out_false$X))))
	}
)

testthat::test_that(
	desc = "Testing collapse_nodal_types",
	code = {
		model <- make_model("X -> Y")
		expect_equal(model$nodal_types, gbiqq:::collapse_nodal_types(model$nodal_types))
		nodal_types <- get_nodal_types(model, collapse = FALSE)
    expect_true(all(grepl("X", gbiqq:::collapse_nodal_types(nodal_types, include_node_names = TRUE)$X)))

		x <- gbiqq:::collapse_nodal_types(nodal_types)$X
		expect_setequal(x, c("0", "1"))

		model <- make_model("X -> Y")
		nodal_types <- get_nodal_types(model, collapse = TRUE)
		x <- gbiqq:::collapse_nodal_types(nodal_types)
		expect_identical(x, nodal_types)

	}
)


