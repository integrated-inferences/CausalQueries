





context(desc = "Testing layering")

testthat::skip_on_cran()
models <- c("X -> Y",
						"X -> Y <- M",
						"C -> Y <- M; X -> Y ")

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






testthat::test_that(

	desc = "Layering restrictions and P matrix",

	code = {


		# Removing columns from P after restrictions
		model_a <- make_model("X->Y") %>%
			set_parameter_matrix() %>%
			set_restrictions("X[]==1")

		model_b <- make_model("X->Y") %>%
			set_restrictions("X[]==1") %>%
			set_parameter_matrix()

		expect_equal(ncol(model_a$P), ncol(model_b$P))
		})


