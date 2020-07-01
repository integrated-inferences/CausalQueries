






context(desc = "Testing type lookup")

	testthat::test_that(

		desc = "Look up nodes",
testthat::skip_on_cran()
		code = {

			model <- make_model('X->Y')
			XMY <- make_model("X->M->Y")

			a <- map_query_to_nodal_type(model, '(Y[X=0] > Y[X=1])')
			expect_true( all(a$types == c(FALSE, TRUE, FALSE, FALSE)))

			query <- 'X == 1'
			expect_error(map_query_to_nodal_type(model, query))


			query <- '(Y[] == 1)'
			x <- map_query_to_nodal_type(model, query, join_by = '|')
			expect_true( all(x$types == c(FALSE, TRUE, TRUE, TRUE)))

			x <- map_query_to_nodal_type(model, query, join_by = '&')
			expect_true( all(x$types == c(FALSE, FALSE, FALSE, TRUE)))

			x <- map_query_to_nodal_type(model, "X[]==1", join_by = '&')
			expect_true( all(x$types == c(FALSE, TRUE)))

			expect_error(map_query_to_nodal_type(XMY, "(M[X=1]==1 & Y[X=1]==1)"))
			expect_error(map_query_to_nodal_type(XMY, "(M==1)"))
			expect_error(map_query_to_nodal_type(XMY, "(Y[X=1]==1)"))


			expect_true(CausalQueries:::add_dots('Y[]', model) == "Y[X = . ]")
			expect_error(CausalQueries:::add_dots('Z[]', model))

		}
	)


