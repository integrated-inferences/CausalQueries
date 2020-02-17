context("Testing helper functions.")

testthat::test_that(
	desc = "perm  permutations_function",
	code = {
		gbiqq:::perm(3)
		expect_true(nrow(gbiqq:::perm(3)) == 4)
		expect_true(sum(gbiqq:::perm(2:3)) == 30)
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
		expect_true(gbiqq:::gsub_many("abc", "a", "b") == "bbc")
		expect_true(gbiqq:::gsub_many("abc", c("ab", "c"), c("o", "k")) == "ok")
	}
)

testthat::test_that(
	desc = "gsub_many: error",
		expect_error(gbiqq:::gsub_many("abc", c("ab", "c"), c("o")))
)



testthat::test_that(
	desc = "clean_condition: spacing strings",
	code = {
		expect_true(gbiqq:::clean_condition("01") == "0 1")
	}
)


testthat::test_that(
	desc = "interpret_type",
	code = {

		model <- make_model('R -> X; Z -> X; X -> Y')
		a1 <- gbiqq:::interpret_type(model, position = list(X = c(3,4), Y = 1))
		a2 <- gbiqq:::interpret_type(model, condition = c('X | Z=0 & R=1', 'X | Z=0 & R=0'))

		expect_true(all(dim(a1[[2]]) == c(1,4)))
		expect_true(all(dim(a2[[1]]) == c(2,4)))

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

	}
)



testthat::test_that(
	desc = "get_parameter_names",
	code = {
		a <- get_parameter_names(make_model('X->Y'))
		expect_true(a[[1]] == "X.0")
		a <- get_parameter_names(make_model('X->Y'), include_paramset = FALSE)
		expect_true(a[[1]] == "0")
	}
)


testthat::test_that(
	desc = "include_vars",
	code = {

		model <- make_model("X->Y")
		expect_true(gbiqq:::includes_var("X", "X==1"))

		expect_true(!(gbiqq:::includes_var("Y", "X==1")))
		a <- gbiqq:::var_in_query(model, "X==1")

		expect_true(a == "X")
	}
)
