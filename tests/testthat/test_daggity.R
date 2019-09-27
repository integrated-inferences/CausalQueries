
context(desc = "Testing translate dagitty")

models <- c("X -> Y", "X -> Y <- M")
dags   <- c("dag{ X -> Y }", "dag{ M -> Y ; X -> Y }")
for(i in length(models)){
	testthat::test_that(
		desc = "observe works when only complete_data is specified",
		code = {
			m   <- make_model(models[i])
			dag <- translate_dagitty(m)
			obs <- expect_equal(dags[i], dag)
		}
	)
}
