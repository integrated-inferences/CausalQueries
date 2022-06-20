




context("Testing set_restrictions")

testthat::skip_on_cran()
testthat::test_that(

	desc = "Check messages for input.",

	code = {
		model <- make_model("X -> Y")
		expect_error(set_restrictions(model, labels = NULL, statement = NULL))
		expect_error(set_restrictions(model, labels = "X", statement =  "(X == 1)"))
		expect_error(set_restrictions(model, labels = "X", given = c(1,2,3)))
		expect_error(set_restrictions(model, labels = "X", given = list(NA,NA)))
		expect_error(set_restrictions(model, statement = decreasing('X','Y'), givene = list(NA,NA)))

		model <- make_model("X -> Y -> Z; X <-> Z")
		expect_error(set_restrictions(model, labels = "Z", given = "abc"))
	}
)

testthat::test_that(

	desc = "Check output.",

	code = {
		model <- make_model("X -> Y <- Z")
		model$P <- NULL
		model <- set_parameter_matrix(model)
		model$P[, ] <- matrix(0, ncol = ncol(model$P), nrow = nrow(model$P))
		model <- set_restrictions(model, statement =  c("(X[] == 1)", "(Z[] == 1)" ))
		attri <- attributes(model)
		expect_true(attri$restrictions$X == "1")
		model <- make_model("X -> Y <- Z")
		model_1 <- set_restrictions(model, statement = c("(X[] == 1)"))
		model_2 <- set_restrictions(model, statement = c("(Z[] == 1)"))
		expect_equal(attr(model_1, "restrictions")$X,attr(model_2, "restrictions")$Z)

		# sequential addition of restrictions
		model <- make_model("X -> Y <- Z")
		model <- set_restrictions(model, statement = c("(X[] == 1)"))
		model <- set_restrictions(model, statement = c("(Z[] == 1)"))
		expect_true((attr(model, "restrictions")$Z == 1) && (attr(model, "restrictions")$X == 1))

		model <- make_model("X -> Y <- Z")
		model_1 <- set_restrictions(model, statement = c("(X[] == 1)"))
		model_2 <- set_restrictions(model_1, statement = c("(Z[] == 1)"))
		expect_equal(attr(model_1, "restrictions")$X,attr(model_2, "restrictions")$Z)

		model <- make_model("X -> Y -> Z; X <-> Z")
		model <- set_restrictions(model, decreasing('Y','Z'), given = "X.0")
		model <- dplyr::filter(model$parameters_df, given == "X.0")
		expect_true(nrow(model) == 3)

	}
)




context("Test restrict_by_query")

testthat::skip_on_cran()
testthat::test_that(

	desc = "Check errors.",

	code = {
		model <- make_model("X -> M -> Y; Z -> Y")
		statement <- c("X == 1", "Z == 1")
		join_by <- c("&", "|", "AND")
		## too many in join_by
		expect_error(CausalQueries:::restrict_by_query(model = model, statement = statement, join_by = join_by))
		## not logical
		expect_error(CausalQueries:::restrict_by_query(model = model, statement = statement, keep = NULL))

		model <- set_restrictions(model, statement =  c("(X[] == 1)", "(Z[] == 1)" ))
		## nodal_types can't be entirely reduced. Revise conditions for node X
		expect_error(set_restrictions(model, statement = c("(X[] == 0)")))

		## defined givens not in given set
		model <- make_model("X -> Y -> Z; X <-> Z")
		expect_error(CausalQueries:::restrict_by_query(model, decreasing('Y','Z'), given = "abc"))
	}
)

context("Test restrict_by_labels")

testthat::skip_on_cran()
testthat::test_that(

	desc = "Check errors.",

	code = {
		model <- make_model("X -> Y")
		## Z is not in the model
		expect_error(CausalQueries:::restrict_by_labels(model, labels = list(Z = "0")))

		## nodal type does not exist for node
		expect_error(CausalQueries:::restrict_by_labels(model,labels = list(Y = "0101")))

		## defined givens not in given set
		model <- make_model("X -> Y -> Z; X <-> Z")
		expect_error(CausalQueries:::restrict_by_labels(model, labels = list(Z = "01"), given = "abc"))
	}
)

testthat::test_that(

	desc = "Check output.",

	code = {
		model <- make_model("X -> Y")
		model <- CausalQueries:::restrict_by_labels(model, labels = list(X = "0"))
		expect_equal(model$nodal_types$X, "1")
		model <- make_model("X -> Y")
		model <- CausalQueries:::restrict_by_labels(model, labels = list(Y = "00", Y = "01"))
		expect_equal(model$nodal_types$Y, c("10", "11"))
	}
)

context("Test get_type_names")

testthat::skip_on_cran()
testthat::test_that(

	desc = "Check output.",

	code = {
		model <- make_model("X -> Y")
		out <- CausalQueries:::get_type_names(model$nodal_types)
		expect_equal(length(out), 6)
		expect_true(all(is.character(out)))
	}
)

context("Test unpack_wildcard")

testthat::skip_on_cran()
testthat::test_that(

	desc = "Check output.",

	code = {
		n <- 3
		x <- paste0(c(rep("?", n)), collapse = "")
		expect_equal(length(CausalQueries:::unpack_wildcard(x)), 2^n)
		x <- "00"
		expect_equal(CausalQueries:::unpack_wildcard(x), x)
	}
)



