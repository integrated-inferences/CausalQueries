
.runThisTest <- Sys.getenv("RunAllRcppTests") == "yes"

if (.runThisTest) {

context("Query model, query distribution")

testthat::test_that(
	desc = "Proper error messages.",
	code = {


		model <- make_model("X -> Y") %>% update_model(data.frame(X=0, Y=0), refresh = 0)

		query_distribution(model, query = "Y[X=1] - Y[X=0]", verbose = TRUE, using = "priors")
		q <- query_distribution(model, query = "Y[X=1] - Y[X=0]", verbose = TRUE, using = "parameters", parameters = 1:6)
		expect_true(q > 0.0555 & q < 0.0556)

		expect_message(query_model(model, query =  "X==1", given = "X==2"))

		expect_error(query_model(model))
		expect_error(query_model(model, query = 1, queries =2))

		q <- query_model(model, query = "Y[X=1] - Y[X=0]")
		expect_true(is.data.frame(q))

		q <- query_model(model, query = "Y[X=1] - Y[X=0]", using = "parameters", parameters = c(.5, .5, 0, 0, 1, 0))
		expect_true(is.data.frame(q))

		q <- query_model(model, query = "Y[X=1] - Y[X=0]", using = c("priors", "parameters"), parameters = c(.5, .5, 0, 0, 1, 0))
		expect_true(q[2,4] == 1)

		q <-query_model(
		                model,
		                query = list(ATE = "Y[X=1] - Y[X=0]", Share_positive = "Y[X=1] > Y[X=0]"),
		                using = c("parameters", "priors", "posteriors"),
		                expand_grid = TRUE, stats = NULL)
		expect_true(is.data.frame(q))

			}
)
}

