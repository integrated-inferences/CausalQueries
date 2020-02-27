context(desc = "Testing collapse_data")

testthat::test_that(
	desc = "when variables not in the model",
	code = {
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
	desc = "data is all NA",
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
	desc = "data is all NA",
	code = {
		model <- make_model('X->Y')
		data <- data.frame(Y = c(1,0,1,0))
		data <- collapse_data(data, model)
	  expect_true(!c("X") %in% data$strategy)
	}
)



