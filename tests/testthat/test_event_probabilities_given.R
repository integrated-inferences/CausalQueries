context("get_event_probabilities with known parameters")

# X -> Y nodal types: Y.ab is Y(X=0)=a, Y(X=1)=b.
# P(X0Y0) = P(X=0) * (Y.00 + Y.01)
# P(X1Y0) = P(X=1) * (Y.00 + Y.10)
# P(X0Y1) = P(X=0) * (Y.10 + Y.11)
# P(X1Y1) = P(X=1) * (Y.01 + Y.11)

test_that("unconditional event probabilities match parameters exactly", {
  model <- make_model("X -> Y") |>
    set_parameters(
      parameters = c(0.25, 0.75, 0.125, 0.125, 0.25, 0.5),
      param_names = c("X.0", "X.1", "Y.00", "Y.10", "Y.01", "Y.11")
    )
  w <- get_event_probabilities(model)

  expect_identical(rownames(w), c("X0Y0", "X1Y0", "X0Y1", "X1Y1"))
  expect_equal(
    w["X0Y0", "event_probs"],
    0.25 * (0.125 + 0.25)
  )
  expect_equal(
    w["X1Y0", "event_probs"],
    0.75 * (0.125 + 0.125)
  )
  expect_equal(
    w["X0Y1", "event_probs"],
    0.25 * (0.125 + 0.5)
  )
  expect_equal(
    w["X1Y1", "event_probs"],
    0.75 * (0.25 + 0.5)
  )
  expect_equal(sum(w), 1)
})


test_that("given= renormalizes the same parameter-derived events", {
  model <- make_model("X -> Y") |>
    set_parameters(
      parameters = c(0.25, 0.75, 0.125, 0.125, 0.25, 0.5),
      param_names = c("X.0", "X.1", "Y.00", "Y.10", "Y.01", "Y.11")
    )

  p_x0y0 <- 0.25 * (0.125 + 0.25)
  p_x1y0 <- 0.75 * (0.125 + 0.125)
  p_x0y1 <- 0.25 * (0.125 + 0.5)
  p_x1y1 <- 0.75 * (0.25 + 0.5)

  w_x <- get_event_probabilities(model, given = "X==1")
  expect_identical(rownames(w_x), c("X0Y0", "X1Y0", "X0Y1", "X1Y1"))
  expect_equal(w_x["X0Y0", "event_probs"], 0)
  expect_equal(w_x["X0Y1", "event_probs"], 0)
  expect_equal(w_x["X1Y0", "event_probs"], p_x1y0 / 0.75)
  expect_equal(w_x["X1Y1", "event_probs"], p_x1y1 / 0.75)
  expect_equal(sum(w_x), 1)

  w_neq <- get_event_probabilities(model, given = "X!=Y")
  denom <- p_x1y0 + p_x0y1
  expect_equal(w_neq["X0Y0", "event_probs"], 0)
  expect_equal(w_neq["X1Y1", "event_probs"], 0)
  expect_equal(w_neq["X1Y0", "event_probs"], p_x1y0 / denom)
  expect_equal(w_neq["X0Y1", "event_probs"], p_x0y1 / denom)
  expect_equal(sum(w_neq), 1)
})


test_that("given= on a keep=TRUE restriction uses possible events (3.9)", {
  # Keep only Y.00: Y is 0 for both X. Possible events are X0Y0, X1Y0.
  model <- make_model("X -> Y") |>
    set_restrictions(labels = list(Y = "00"), keep = TRUE) |>
    set_parameters(
      parameters = c(0.25, 0.75, 1),
      param_names = c("X.0", "X.1", "Y.00")
    )

  w <- get_event_probabilities(model)
  expect_identical(sort(rownames(w)), c("X0Y0", "X1Y0"))
  expect_equal(w["X0Y0", "event_probs"], 0.25)
  expect_equal(w["X1Y0", "event_probs"], 0.75)

  w_x <- get_event_probabilities(model, given = "X==1")
  expect_identical(rownames(w_x), rownames(w))
  expect_equal(w_x["X0Y0", "event_probs"], 0)
  expect_equal(w_x["X1Y0", "event_probs"], 1)

  expect_error(
    get_event_probabilities(model, given = "Y==1"),
    "No probability mass matches"
  )
})


test_that("given= after dropping a type still matches the nodal-type formula", {
  model <- make_model("X -> Y") |>
    set_restrictions(labels = list(Y = "00")) |>
    set_parameters(
      parameters = c(0.25, 0.75, 0.25, 0.25, 0.5),
      param_names = c("X.0", "X.1", "Y.10", "Y.01", "Y.11")
    )

  p_x0y0 <- 0.25 * 0.25
  p_x1y0 <- 0.75 * 0.25
  p_x0y1 <- 0.25 * (0.25 + 0.5)
  p_x1y1 <- 0.75 * (0.25 + 0.5)

  w <- get_event_probabilities(model)
  expect_equal(w["X0Y0", "event_probs"], p_x0y0)
  expect_equal(w["X1Y0", "event_probs"], p_x1y0)
  expect_equal(w["X0Y1", "event_probs"], p_x0y1)
  expect_equal(w["X1Y1", "event_probs"], p_x1y1)

  w_x <- get_event_probabilities(model, given = "X==1")
  expect_equal(w_x["X0Y0", "event_probs"], 0)
  expect_equal(w_x["X0Y1", "event_probs"], 0)
  expect_equal(w_x["X1Y0", "event_probs"], p_x1y0 / 0.75)
  expect_equal(w_x["X1Y1", "event_probs"], p_x1y1 / 0.75)
})
