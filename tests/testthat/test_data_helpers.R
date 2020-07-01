




context(desc = "Testing data helperse")

testthat::skip_on_cran()
testthat::test_that(

	desc = "collapse_data works when variables not in the model",

			code = {
				 model <- make_model('X->Y')
				 long_data <- simulate_data(model, n = 6)
				 data <- collapse_data(long_data, model)
				 expect_true(class(data) == "data.frame")
				 expect_true(unique(data$strategy) == "XY")

				 model <- make_model('X->Y')
				 data <- simulate_data(model, n = 3)
				 Z <- c(0, 0, 0)
				 data <- cbind(data, Z)
				 nodes <- model$nodes
				 data <- collapse_data(data, model)
				 expect_equal(nrow(data), 4)

	}
)




testthat::test_that(

	desc = "collapse_data works when all NA",

	code = {
		model <- make_model('X->Y')
		X <- c(NA, NA, NA)
		Y <- c(NA, NA, NA)
		data <- as.data.frame(cbind(X, Y))
		data <- collapse_data(data, model)
		expect_equal(nrow(data), 4)
	}
)

testthat::test_that(

	desc = "collapse_data works when node not in data",

	code = {
		model <- make_model('X->Y')
		data <- data.frame(Y = c(1,0,1,0))
		data <- collapse_data(data, model)
	  expect_true(!c("X") %in% data$strategy)
	}
)

testthat::test_that(

	desc = "collapse data inconsistent with model and ignored",

	code = {
		model <- make_model('X -> Y') %>% set_restrictions('X[]==1')
		data <- data.frame(X= 0:1)
		expect_message(collapse_data(data, model))
	}
)


testthat::test_that(

	desc = "collapse_data conditions work",

	code = {
		model <- make_model('X -> Y')
		data <- simulate_data(model, n = 4)%>%
						collapse_data( model, drop_family = TRUE)
		expect_true(!"strategy" %in% colnames(data))
		data <- simulate_data(model, n = 4)%>%
						collapse_data( model, summary = TRUE)
		expect_true(class(data) == "list")
		expect_equal(names(data), c("data_events","observed_events", "unobserved_events"))
	}
)


testthat::test_that(

	desc = "expand_data works",

	code = {
		model <-  make_model('X -> Y')
		events <- make_events(model, n = 4)
		data <- expand_data(events, model)
		expect_equal(nrow(data), 4)
		expect_equal(names(data), c("X", "Y"))
		data <- expand_data(model = model)
	 }
)

testthat::test_that(

	desc = "expand_data errors",

	code = {
		model <-  make_model('X -> Y')
		events <- make_events(model, n = 5)
		expect_error(expand_data(events, model = 'X -> Y'))
		expect_error(expand_data(c(events = c("X0YO", "X1Y1")), model))
		colnames(events) <- NULL
		expect_error(expand_data(events, model))
	}
)

testthat::test_that(

	desc = "all_data_types errors",

	code = {
		model <- make_model("X -> Y")
		expect_error(all_data_types(model, given = "Z == 0"))
		model <- make_model('X -> Y') %>%
			set_restrictions(labels = list(Y = '00'), keep = TRUE)
		out <- all_data_types(model, given = "Y == 0")
		expect_true(is.na(out$X[3]))
	}
)



testthat::test_that(

	desc = "get_data_families works",

	code = {
		model <- make_model("X -> Y")
		expect_equal(nrow(get_data_families(model)),  8)

		model <- make_model('X->Y') %>%
			set_restrictions(statement =  '(Y[X = .] == 1)', join_by = '&', keep = TRUE)
    data_fam <- get_data_families(model)
		expect_true(!"Y0"%in% data_fam$event)

		model <- model %>%
						 set_restrictions(statement =  'X[] == 1', keep = TRUE)
		data_fam <- get_data_families(model)
		expect_equal(ncol(data_fam), 3)
		expect_equal(data_fam$event, c("X1Y1", "Y1", "X1"))
	}
)





