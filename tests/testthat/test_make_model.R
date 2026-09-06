context("Testing make_model")

testthat::skip_on_cran()

testthat::test_that(

	desc = "Print and summary functions",

	code = {
		model <- make_model("X -> Y")
		out <- capture.output(print(model))
		expect_true(any(grepl("X -> Y", out)) &
		              any(grepl("^Number of nodal types by node:", out)))
		# Causal-type count is printed only when causal_types are attached
		# (legacy / on-demand). Factorized make_model omits that block.
		if (!is.null(model$causal_types)) {
		  expect_true(any(grepl("^Number of causal types:", out)))
		}
		out <- class(summary(model))
		expect_equal(out, "summary.causal_model")

		model <- update_model(model)
		out <- capture.output(print(model))
		expect_no_warning(print(summary(model), stanfit = TRUE))
		expect_true(any(grepl("Model has been updated.+", out)))

		model <- make_model("X -> Y") |> set_confound(list("X <-> Y"))
		model <- make_model("X->Y") |> set_restrictions(statement = c("X[] == 0"))
		out <- capture.output(print(summary(model)))
		expect_true(any(grepl("Restrictions.+", out)))
 	}
)



testthat::test_that(

  desc = "Check errors",

  code = {
    expect_error(make_model("X -> S <- Y; S <-> Z"))
    expect_error(make_model(c("X -> Y" ,  "M -> Y")))
    expect_error(make_model(1))
    expect_error(make_model("X ->"))
    expect_error(make_model("X -> -> Y"))
    expect_error(make_model("institutions -> political_inequality"))
    expect_error(make_model("political-inequality <- institutions"))
    expect_error(make_model("institutions -> political>inequality"))
    expect_error(make_model("institutions -> political<inequality"))
    expect_error(make_model("X.exp( -> Y"))
    expect_error(make_model("X.log( -> Y"))
    expect_error(make_model("X^Y -> Y"))
    expect_error(make_model("X/Y -> Y"))
    expect_error(make_model("X[Y] -> Y"))
    expect_error(make_model("X:|:Y -> Y"))
    expect_error(make_model("X -> Y; Y -> X"))
    expect_error(make_model("X -> M; M -> W; W -> X; Z -> Y"))
  }
)


testthat::test_that(

  desc = "Nodal types",

  code = {
    expect_error(make_model("X -> Y" , nodal_types = list(Z = c("0", "1"))))
    expect_error(make_model("X -> Y" , nodal_types = list(Y = c("0", "1"))))
    expect_message(make_model("X -> Y" ,
                              nodal_types = list(
                                Y = c("00", "01", "10", "11"),
                                X = c("0", "1"))
                              ))
    expect_message(make_model("X -> Y" , nodal_types = FALSE))
    expect_message(make_model("Z -> Y",
                              nodal_types = list(
                                Y = c("01", "10"),
                                Z = c("0", "1"))
                              ))
  }
)



testthat::test_that(

  desc = "Guard against too many causal types",

  code = {
    # five parents with auto-generated nodal types is refused before any
    # type is built. Without the guard make_model() can allocate tens of GB,
    # so fail loudly if the helpers are missing from the installed package.
    if (!exists("check_causal_type_count",
                envir = asNamespace("CausalQueries"),
                inherits = FALSE)) {
      fail(paste("check_causal_type_count() is absent from the CausalQueries",
                 "namespace, so the model size guard is missing. Reinstall the",
                 "package from source before running these tests."))
    } else {
      expect_error(make_model("A->Y; B->Y; C->Y; D->Y; E->Y"),
                   "too many nodal types")
      expect_error(make_model("A->Y; B->Y; C->Y; D->Y; E->Y",
                              allow_large = TRUE),
                   "too many nodal types")
    }

    # four binary parents imply 2^4 * 65536 = 1,048,576 causal types
    four_parent_types <- c(A = 2, B = 2, C = 2, D = 2, Y = 65536)

    expect_error(
      CausalQueries:::check_causal_type_count(four_parent_types),
      "causal types")
    expect_error(
      CausalQueries:::check_causal_type_count(four_parent_types),
      "allow_large = TRUE")
    expect_error(
      CausalQueries:::check_causal_type_count(four_parent_types),
      "add_causal_types = FALSE")

    expect_warning(
      CausalQueries:::check_causal_type_count(four_parent_types,
                                              allow_large = TRUE),
      "causal types")

    # below the soft limit
    expect_silent(
      CausalQueries:::check_causal_type_count(c(A = 2, B = 2, C = 2, Y = 256)))

    # add_causal_types = FALSE skips the product check even above the limit
    expect_silent(
      CausalQueries:::check_causal_type_count(four_parent_types,
                                              add_causal_types = FALSE))

    # restricted nodal_types: many parents but small product is allowed
    expect_no_error(
      make_model("A -> Y; B ->Y; C->Y; D->Y; E->Y",
                 nodal_types = list(
                   A = c("0", "1"),
                   B = c("0", "1"),
                   C = c("0", "1"),
                   D = c("0", "1"),
                   E = c("0", "1"),
                   Y = c("00000000000000000000000000000000",
                         "11111111111111111111111111111111"))))

    # supplied nodal_types whose product exceeds the limit still need allow_large
    big_types <- list(
      A = c("0", "1"),
      B = as.character(seq_len(1000)),
      Y = as.character(seq_len(2000))
    )
    # product = 2 * 1000 * 2000 = 4e6
    expect_error(
      CausalQueries:::check_causal_type_count(lengths(big_types)),
      "causal types")
  }
)


testthat::test_that(

  desc = "Clean statement",

  code = {
    expect_equal(make_model("X -> Y<-  X") |> grab("statement"), "X -> Y")
    expect_equal(make_model("X -> Y; X<->Y; Y<->X") |> grab("statement"), "X -> Y; X <-> Y")
  }
)

