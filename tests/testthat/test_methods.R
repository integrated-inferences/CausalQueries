
context("Print and Summary methods")

testthat::skip_on_cran()
testthat::test_that(

  desc = "Proper summaries and print.",

  code = {

    # Print methods
    model <- make_model("X->Y")
    out <- capture.output(print(grab(model, object = "nodes")))
    expect_true(any(grepl("Nodes:", out)))

    out <- capture.output(print(grab(model, object = "parents_df")))
    expect_true(any(grepl("parents", out)))

    out <- capture.output(print(grab(model, object = "parameters_df")))
    expect_false(any(grepl("first 10 rows:", out)))
    model <- make_model("X -> Y <- M; X -> M")
    out <- capture.output(print(grab(model, object = "parameters_df")))
    expect_true(any(grepl("first 10 rows:", out)))

    out <- capture.output(print(grab(model, object = "causal_types")))
    expect_true(any(grepl("first 10 causal types:", out)))
    model <- make_model("X->Y")
    out <- capture.output(print(grab(model, object = "causal_types")))
    expect_false(any(grepl("first 10 causal types:", out)))


    out <- capture.output(print(grab(model, object = "nodal_types")))
    expect_false(any(grepl("types omitted", out)))
    model <- make_model("X -> Y <- M; W -> Y")
    out <- capture.output(print(grab(model, object = "nodal_types")))
    expect_true(any(grepl("types omitted", out)))

    model <- make_model("X->Y")
    out <- capture.output(print(grab(model, object = "parameters")))
    expect_true(any(grepl("Model parameters with associated probabilities", out)))

    out <- capture.output(print(grab(model, object = "prior_distribution")))
    expect_true(any(grepl("Summary:", out)))

    model <- update_model(model,  keep_event_probabilities = TRUE)
    out <- capture.output(print(grab(model, object = "posterior_distribution")))
    expect_true(any(grepl("posterior distributions", out)))


    out <- capture.output(print(grab(model, object = "posterior_distribution")))
    expect_true(any(grepl("posterior distributions", out)))

    out <- capture.output(print(grab(model, object = "type_prior")))
    expect_true(any(grepl("type prior", out)))

    out <- capture.output(print(grab(model, object = "type_prior")))
    expect_true(any(grepl("type prior", out)))

    out <- capture.output(print(grab(model, object = "stan_summary")))
    expect_true(any(grepl("Inference for Stan model", out)))

    out <- capture.output(print(grab(model, object = "posterior_event_probabilities")))
    expect_true(any(grepl("event probabilities", out)))

    out <- capture.output(print(grab(model, object = "event_probabilities")))
    expect_true(any(grepl("event_probs", out)))

    out <- capture.output(print(grab(model, object = "type_distribution")))
    expect_true(any(grepl("Posterior draws", out)))

    out <- capture.output(print(   query_model(model, "Y[X=1] - Y[X = 0]", using = "parameters")) )
    expect_true(any(grepl("Causal queries", out)))


  }



)
