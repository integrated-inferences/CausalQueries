





context("Testing get_nodal_types")

testthat::skip_on_cran()
testthat::test_that(
  desc = "Condition for make_nodal_types",
  code = {
    out_true <- CausalQueries:::make_nodal_types(make_model("X -> Y"),
                                                 include_node_names = TRUE)
    expect_true(all(grepl("X", rownames(out_true$X))))
    out_false <- CausalQueries:::make_nodal_types(make_model("X -> Y"),
                                                  include_node_names = FALSE)
    expect_false(any(grepl("X", rownames(out_false$X))))
  })

testthat::test_that(
  desc = "Testing collapse_nodal_types",
  code = {
    model <- make_model("X -> Y")
    expect_equal(model$nodal_types,
                 CausalQueries:::collapse_nodal_types(model$nodal_types))
    nodal_types <- get_nodal_types(model, collapse = FALSE)
    expect_true(all(grepl(
      "X",
      CausalQueries:::collapse_nodal_types(nodal_types,
                                           include_node_names = TRUE)$X
    )))

    x <- CausalQueries:::collapse_nodal_types(nodal_types)$X
    expect_setequal(x, c("0", "1"))

    model <- make_model("X -> Y")
    nodal_types <-
      get_nodal_types(model, collapse = TRUE)
    x <- CausalQueries:::collapse_nodal_types(nodal_types)
    expect_identical(x, nodal_types)

  })


