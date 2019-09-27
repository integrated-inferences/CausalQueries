


context(desc = "Testing layering")

models <- c("X -> Y", "X -> Y <- M", "C -> Y <- M; X -> Y ")

restrictions <- c("Y[X=0] == 1", "Y[X=0, M = 0] > Y[X=1, M = 1]", "Y[X = 0, C = 0, M= 0] == 1")


for(i in length(models)){
	testthat::test_that(
		desc = "Layering confounds and restrictions work in both directions",
		code = {

			model1 <- make_model(models[i]) %>%
				set_confound(list(X = "Y[X=1]==1"))%>%
				set_restrictions(restrictions[i])

			model2 <- make_model(models[i])%>%
			set_restrictions(restrictions[i]) %>%
			set_confound(list(X = "Y[X=1]==1"))

			expect_equal(nrow(get_causal_types(model1)), nrow(get_causal_types(model2)))


			model1 <- make_model(models[i]) %>%
				set_confound(list(X = "Y[X=1]==1"))%>%
				set_restrictions(restrictions[i], keep = TRUE)

			model2 <- make_model(models[i])%>%
				set_restrictions(restrictions[i], keep = TRUE) %>%
				set_confound(list(X = "Y[X=1]==1"))

			expect_equal(nrow(get_causal_types(model1)), nrow(get_causal_types(model2)))


		}
	)
}





