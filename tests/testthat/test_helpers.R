



context("Testing helper functions.")

testthat::skip_on_cran()
testthat::test_that(

	desc = "perm  permutations_function",

	code = {
		CausalQueries:::perm(3)
		expect_true(nrow(CausalQueries:::perm(3)) == 4)
		expect_true(sum(CausalQueries:::perm(2:3)) == 30)
	}
)

testthat::test_that(

	desc = "st_within: Removing consecutive brackets",

	code = {
		# tests for remove
		b <- "(XXX[[Y=0]] == 1 + XXX[[Y=1]] == 0)"
		expect_equal(st_within(b), c("XXX", "XXX"))
		expect_error(st_within(3))

	}
)

testthat::test_that(

	desc = "gsub_many: multiple substitutions",

	code = {
		expect_true(CausalQueries:::gsub_many("abc", "a", "b") == "bbc")
		expect_true(CausalQueries:::gsub_many("abc", c("ab", "c"), c("o", "k")) == "ok")
	}
)

testthat::test_that(

	desc = "gsub_many: error",

		expect_error(CausalQueries:::gsub_many("abc", c("ab", "c"), c("o")))
)

testthat::test_that(

	desc = "clean_condition: spacing strings",

	code = {
		expect_true(CausalQueries:::clean_condition("01") == "0 1")
	}
)

testthat::test_that(

	desc = "interpret_type",

	code = {
		model <- make_model('R -> X; Z -> X; X -> Y')
		a1 <- CausalQueries:::interpret_type(model, position = list(X = c(3,4), Y = 1))
		a2 <- CausalQueries:::interpret_type(model, condition = c('X | Z=0 & R=1', 'X | Z=0 & R=0'))
		expect_true(all(dim(a1[[2]]) == c(1,4)))
		expect_true(all(dim(a2[[1]]) == c(2,4)))
		# both defined
		expect_error(interpret_type(model, condition = c('X | Z=0 & R=1', 'X | Z=0 & R=0'), position = list(X = c(3,4), Y = 1)))
		# not in types
		expect_error(CausalQueries:::interpret_type(model, position = list(Q = c(3,5), Y = 1)))
	}
)

testthat::test_that(

	desc = "expand_wildcard",

	code = {
		expect_message(expand_wildcard("Y[X=1, M=.] ==1"))

		a <- expand_wildcard('(Y[X=1, M=1] > Y[X=1, M=0])')== "(Y[X=1, M=1] > Y[X=1, M=0])"
		expect_true(a)

		a <- expand_wildcard('(Y[X=1, M=.] > Y[X=1, M=.])', verbose = FALSE)
		expect_true(a[[1]] == "(Y[X=1, M=0] > Y[X=1, M=0] | Y[X=1, M=1] > Y[X=1, M=1])")

		a2 <- expand_wildcard('(Y[X=1, M=.] > Y[X=1, M=.])', join_by = "&", verbose = TRUE)
		expect_true(a2[[1]] == "(Y[X=1, M=0] > Y[X=1, M=0] & Y[X=1, M=1] > Y[X=1, M=1])")

		a2 <- expand_wildcard('(Y[X=.]==1 & Y[X=.]==0)', join_by = "&", verbose = TRUE)
		expect_true(a2[[1]] == "(Y[X=0]==1 & Y[X=0]==0 & Y[X=1]==1 & Y[X=1]==0)")

		out <- capture.output(expand_wildcard('(Y[X=.]==1)', join_by = NULL, verbose = TRUE))
		expect_equal(sum(grepl("\\(Y\\[X=.\\]==1\\)", out)), 3)
	}
)

testthat::test_that(

	desc = "get_parameter_names",

	code = {
		out <- get_parameter_names(make_model('X->Y'), include_paramset = FALSE)
		expect_true(all(!grepl("X|Y", out)))
	}
)



testthat::test_that(

  desc = "is_a_model",

  code = {
    model_1 <- model_2 <-  make_model("X->Y")
    model_2$dag <- NULL
    model_3 <- "THIS"

    expect_no_error(CausalQueries:::is_a_model(model_1))
    expect_error(CausalQueries:::is_a_model(model_2))
    expect_error(CausalQueries:::is_a_model(model_3))

  }
)


testthat::test_that(
  desc = "check_query",
  code = {
    query =
    expect_no_error(CausalQueries:::check_query("D[C=B[A=1]] == 1"))
    expect_warning(CausalQueries:::check_query("D[C=B[A=1] == 1"))
  }
)





