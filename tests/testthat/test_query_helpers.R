context("Tests for query_helpers")

testthat::test_that(
	desc = "Increasing, non-decreasing, ",
	code = {
		out <- increasing("A", "B")
		expect_equal(out[1], "(B[A=1] > B[A=0])")
		expect_equal(attributes(out)$class, "statement")
		out <- non_decreasing("A", "B")
		expect_equal(out[1], "(B[A=1] >= B[A=0])")
	}
)
