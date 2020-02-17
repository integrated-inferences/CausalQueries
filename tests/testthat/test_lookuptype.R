


context(desc = "Testing type lookup")

	testthat::test_that(
		desc = "Look up nodes",
		code = {

			model <- make_model('X->Y')
			XMY <- make_model("X->M->Y")

			a <- lookup_nodal_type(model, '(Y[X=0] > Y[X=1])')
			expect_true( all(a$types == c(FALSE, TRUE, FALSE, FALSE)))

			query <- 'X == 1'
			expect_error(lookup_nodal_type(model, query))


			query <- '(Y[] == 1)'
			x <- lookup_nodal_type(model, query, join_by = '|')
			expect_true( all(x$types == c(FALSE, TRUE, TRUE, TRUE)))

			x <- lookup_nodal_type(model, query, join_by = '&')
			expect_true( all(x$types == c(FALSE, FALSE, FALSE, TRUE)))

			x <- lookup_nodal_type(model, "X[]==1", join_by = '&')
			expect_true( all(x$types == c(FALSE, TRUE)))

			expect_error(lookup_nodal_type(XMY, "(M[X=1]==1 & Y[X=1]==1)"))
			expect_error(lookup_nodal_type(XMY, "(M==1)"))
			expect_error(lookup_nodal_type(XMY, "(Y[X=1]==1)"))


			expect_true(gbiqq:::add_dots('Y[]', model) == "Y[X = . ]")
			expect_error(gbiqq:::add_dots('Z[]', model))

		}
	)
