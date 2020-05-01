
.runThisTest <- Sys.getenv("RunAllRcppTests") == "yes"

if (.runThisTest) {

context("Tests for type_helpers")

testthat::test_that(
	desc = "Testing add_wildcard",
	code = {
		expect_equal(add_wildcard("A", "R[B = 1] -> A", c("C", "D"), c("C", "R")), "R[B = 1, C = . , R = . ] -> A")
		expect_equal(add_wildcard("A", "R[B = 1] -> A", c("C", "R"), c("C", "R")), "A[C = . , R = . ]")
	}
)
}
