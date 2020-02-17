context("Testing set_restrictions")

testthat::test_that(
	desc = "Check messages for input.",
	code = {
		model <- make_model("X -> Y")
		expect_message(set_restrictions(model, labels = NULL, statement = NULL))
		expect_message(set_restrictions(model, labels = "X", statement =  "(X == 1)"))
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
	}
)

context("Test restrict_by_query")

testthat::test_that(
	desc = "Check errors.",
	code = {
		model <- make_model("X -> M -> Y; Z -> Y")
		statement <- c("X == 1", "Z == 1")
		join_by <- c("&", "|", "AND")
		## too many in join_by
		expect_error(gbiqq:::restrict_by_query(model = model, statement = statement, join_by = join_by))
		model <- set_restrictions(model, statement =  c("(X[] == 1)", "(Z[] == 1)" ))
		## nodal_types can't be entirely reduced. Revise conditions for node X
		expect_error(set_restrictions(model, statement = c("(X[] == 0)")))
	}
)

context("Test restrict_by_labels")

testthat::test_that(
	desc = "Check errors.",
	code = {
		model <- make_model("X -> Y")
		## Z is not in the model
		expect_error(gbiqq:::restrict_by_labels(model, labels = list(Z = "0")))
	}
)

testthat::test_that(
	desc = "Check output.",
	code = {
		model <- make_model("X -> Y")
		model <- gbiqq:::restrict_by_labels(model, labels = list(X = "0"))
		expect_equal(model$nodal_types$X, "1")
		model <- make_model("X -> Y")
		model <- gbiqq:::restrict_by_labels(model, labels = list(Y = "00", Y = "01"))
		expect_equal(model$nodal_types$Y, c("10", "11"))
	}
)

context("Test get_type_names")

testthat::test_that(
	desc = "Check output.",
	code = {
		model <- make_model("X -> Y")
		out <- gbiqq:::get_type_names(model$nodal_types)
		expect_equal(length(out), 6)
		expect_true(all(is.character(out)))
	}
)

context("Test unpack_wildcard")

testthat::test_that(
	desc = "Check output.",
	code = {
		n <- 3
		x <- paste0(c(rep("?", n)), collapse = "")
		expect_equal(length(gbiqq:::unpack_wildcard(x)), 2^n)
		x <- "00"
		expect_equal(gbiqq:::unpack_wildcard(x), x)
	}
)



