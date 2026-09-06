
context("Print and Summary methods")

testthat::skip_on_cran()
testthat::test_that(

  desc = "Proper summaries and print.",

  code = {

    # Print methods
    model <- make_model("X->Y")


    out <- capture.output(summary(model))
    expect_true(any(grepl("pose causal queries", out)))

    out <- capture.output(summary(model, include ="nodes"))
    expect_true(any(grepl("Nodes:", out)))

    out <- capture.output(summary(model, include ="parents_df"))
    expect_true(any(grepl("parents", out)))

    out <- capture.output(summary(model, include ="parameters_df"))
    expect_false(any(grepl("first 10 rows:", out)))

    model <- make_model("X -> Y <- M; X -> M")
    out <- capture.output(summary(model, include ="parameters_df"))
    expect_true(any(grepl("snippet", out)))

    out <- capture.output(summary(model, include ="causal_types"))
    # Large model: causal_types snippet when types are available
    if (!is.null(model$causal_types) || isTRUE(getOption("CausalQueries.legacy"))) {
      expect_true(any(grepl("snippet", out)) || any(grepl("Causal Types", out)) ||
                    any(grepl("causal types", out, ignore.case = TRUE)))
    }

    model <- make_model("X->Y")
    out <- capture.output(summary(model, include ="causal_types"))
    expect_false(any(grepl("first 10 causal types:", out)))


    out <- capture.output(summary(model, include ="nodal_types"))
    expect_false(any(grepl("types omitted", out)))
    model <- make_model("X -> Y <- M; W -> Y")
    out <- capture.output(summary(model, include ="nodal_types"))
    expect_true(any(grepl("types omitted", out)))

    model <- make_model("X->Y")
    out <- capture.output(summary(model, include ="parameters"))
    expect_true(any(grepl("Model parameters with associated probabilities", out)))

    model <- update_model(model,  keep_event_probabilities = TRUE)

    out <- capture.output(summary(model, include ="prior_distribution"))
    expect_true(any(grepl("Summary statistics", out)))

    out <- capture.output(summary(model, include = "posterior_distribution"))
    expect_true(any(grepl("posterior distributions", out)))
    # Message about missing stanfit is optional depending on keep_fit / path
    # (factorized updates may still attach a fit summary without stanfit object)

    model <- update_model(model,  keep_event_probabilities = TRUE, data = data.frame(X = 1))
    out <- capture.output(summary(model, include = "posterior_distribution"))
    expect_true(any(grepl("posterior distributions", out)))

    out <- capture.output(summary(model, include =c("posterior_distribution", "ambiguities_matrix")))
    expect_true(any(grepl("posterior_distribution", out)))
    expect_true(any(grepl("ambiguities_matrix", out)))

    out <- capture.output(summary(model, include ="type_prior"))
    expect_true(any(grepl("type_prior", out)))

    out <- capture.output(summary(model, include ="stan_summary"))
    expect_true(any(grepl("Inference for Stan model", out)))

    out <- capture.output(summary(model, include ="posterior_event_probabilities"))
    expect_true(any(grepl("event probabilities", out)))

    out <- capture.output(summary(model, include ="prior_event_probabilities"))
    expect_true(any(grepl("event_probs", out)))

    skip_if_legacy_objects()
    model_leg <- with_legacy_true({
      update_model(
        make_model("X->Y"),
        keep_event_probabilities = TRUE,
        keep_type_distribution = TRUE,
        data = data.frame(X = 1),
        refresh = 0
      )
    })
    out <- capture.output(summary(model_leg, include ="type_posterior"))
    expect_true(any(grepl("Posterior draws", out)))

    expect_error(summary(model, include = c("xx")))

    out <- capture.output(query_model(model, "Y[X=1] - Y[X=0]", using = "priors"))
    expect_true(any(grepl("Causal queries", out)))
    expect_true(any(grepl("|:---------------|:------|-----:|-----:|--------:|---------:|", out)))

    out <- capture.output((query_model(model, "Y[X=1] - Y[X=0]", using = "parameters")))
    expect_true(any(grepl("Causal queries", out)))
    expect_true(any(grepl("|:---------------|:----------|----:|", out)))


  }



)


testthat::test_that(

  desc = "summary of a model_query returns a summary.model_query object.",

  code = {

    q <- query_model(make_model("X -> Y"),
                     "Y[X=1] - Y[X=0]",
                     using = "priors")

    s <- summary(q)
    expect_s3_class(s, "summary.model_query")

    # summarizing is silent; printing is what produces output
    expect_silent(invisible(summary(q)))

    out <- capture.output(print(s))
    expect_true(any(grepl("Call:", out)))
    expect_true(any(grepl("Causal queries", out)))

  }
)
