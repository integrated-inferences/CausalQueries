context("Testing helper functions.")

testthat::test_that(
	desc = "st_within: Removing consecutive brackets",
	code = {
		# tests for remove
		b <- "(XXX[[Y=0]] == 1 + XXX[[Y=1]] == 0)"
		expect_equal(st_within(b), c("XXX", "XXX"))
	}
)


testthat::test_that(
	desc = "perm: permutations_function",
	code = {
		expect_true(nrow(perm(3)) == 4)
		expect_true(sum(perm(2:3)) == 30)
	}
)


testthat::test_that(
	desc = "gsub_many: multiple substitutions",
	code = {
		expect_true(gbiqq:::gsub_many("abc", "a", "b") == "bbc")
		expect_true(gbiqq:::gsub_many("abc", c("ab", "c"), c("o", "k")) == "ok")
		expect_error(gbiqq:::gsub_many("abc", c("ab", "c"), c("o")) == "ok")
	}
)


