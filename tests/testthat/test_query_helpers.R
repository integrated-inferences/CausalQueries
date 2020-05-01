
.runThisTest <- Sys.getenv("RunAllRcppTests") == "yes"

if (.runThisTest) {

context("Tests for query_helpers")

testthat::test_that(
	desc = "All functions:",
	code = {
		out <- increasing("A", "B")
		expect_equal(out[1], "(B[A=1] > B[A=0])")
		expect_equal(attributes(out)$class, "statement")
		out <- non_decreasing("A", "B")
		expect_equal(out[1], "(B[A=1] >= B[A=0])")
		out <- decreasing("A", "B")
		expect_equal(out[1], "(B[A=1] < B[A=0])")
		out <- non_increasing("A", "B")
		expect_equal(out[1], "(B[A=1] <= B[A=0])")
		out <- interacts("X", "W", "Y")
		expect_equal(out[1], "((Y[X =1, W = 1]) - (Y[X = 0, W = 1])) != ((Y[X =1, W = 0]) - (Y[X = 0, W = 0]))")
		out <- complements("X", "W", "Y")
		expect_equal(out[1], "((Y[X =1, W = 1]) - (Y[X = 0, W = 1])) > ((Y[X =1, W = 0]) - (Y[X = 0, W = 0]))")
		out <- substitutes("X", "W", "Y")
		expect_equal(out[1], "((Y[X = 1, W = 1]) - (Y[X = 0, W = 1])) < ((Y[X = 1, W = 0]) - (Y[X = 0, W = 0]))")
		out <- te("A", "B")
		expect_equal(out[1], "(B[A=1] - B[A=0])")
	}
)


testthat::test_that(
	desc = "Check input.",
	code = {
		expect_error(CausalQueries:::te(2, 1))
	}
)




testthat::test_that(
	desc = "Test error",
	code = {
		expect_error(increasing(1, "B"))
	}
)


}
