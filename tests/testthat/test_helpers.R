context("Testing helper functions.")

testthat::test_that(
	desc = "st_within: Removing consecutive brackets",
	code = {
		# tests for remove
		b <- "(XXX[[Y=0]] == 1 + XXX[[Y=1]] == 0)"
		expect_equal(st_within(b), c("XXX", "XXX"))
	}
)
