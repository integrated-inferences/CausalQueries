context("get_data_families E matrix (multiply vs nested apply)")

# Reference: previous nested-apply construction of E only.
# Kept here so the multiply form cannot drift from the contract that
# Stan relies on: rows = observed / coarsened events (including NA
# strategies), columns = possible complete data types, w_full = E * w.

E_from_apply <- function(model) {
  nodes <- model$nodes
  all_data <- CausalQueries:::get_all_data_types(model)
  possible_data_types <- unique(
    CausalQueries:::data_type_names(model, realise_outcomes(model))
  )
  full_data <- all_data[
    !apply(all_data[nodes], 1, function(j) any(is.na(j))),
    ,
    drop = FALSE
  ]
  full_data <- full_data[full_data$event %in% possible_data_types, , drop = FALSE]

  sign_matrix <- (2 * as.matrix(all_data[nodes]) - 1)
  sign_matrix[is.na(sign_matrix)] <- 0
  type_matrix <- (2 * as.matrix(full_data[nodes]) - 1)

  E <- 1 * matrix(
    apply(sign_matrix, 1, function(j) {
      apply(type_matrix, 1, function(k) !(any(k * j == -1)))
    }),
    nrow = length(all_data$event),
    byrow = TRUE
  )
  rownames(E) <- all_data$event
  colnames(E) <- full_data$event
  E
}

families_equal <- function(model) {
  got <- CausalQueries:::get_data_families(model, mapping_only = TRUE)
  expect <- E_from_apply(model)
  # drop_impossible / drop_all_NA as in get_data_families
  keep <- rep(TRUE, nrow(expect))
  keep[rowSums(expect) == 0] <- FALSE
  keep[rownames(expect) == "None"] <- FALSE
  expect <- expect[keep, , drop = FALSE]
  identical(got, expect)
}

test_that("E matches nested apply on complete X -> Y", {
  model <- make_model("X -> Y")
  expect_true(families_equal(model))
  fam <- CausalQueries:::get_data_families(model)
  # Full observed partition: complete types plus single-node and empty strategies
  expect_true(all(c("X0Y0", "X1Y0", "X0Y1", "X1Y1", "X0", "X1", "Y0", "Y1") %in% fam$event))
  expect_false("None" %in% fam$event)
  map <- CausalQueries:::get_data_families(model, mapping_only = TRUE)
  expect_true(all(map["X0Y0", ] %in% c(0, 1)))
  expect_equal(sum(map["X0Y0", ]), 1)
  expect_equal(colnames(map)[map["X0Y0", ] == 1], "X0Y0")
  # Coarsened Y0 is consistent with both complete types that have Y=0
  expect_equal(sum(map["Y0", ]), 2L)
  expect_true(all(c("X0Y0", "X1Y0") %in% colnames(map)[map["Y0", ] == 1]))
})

test_that("E matches nested apply with missingness strategies on a chain", {
  model <- make_model("X -> M -> Y")
  expect_true(families_equal(model))
  map <- CausalQueries:::get_data_families(model, mapping_only = TRUE)
  # NA strategies must remain as rows
  expect_true(any(grepl("^X[01]$", rownames(map))))
  expect_true(any(grepl("^Y[01]$", rownames(map))))
  expect_true(any(grepl("^M[01]$", rownames(map))))
  # A coarsened event is consistent with every complete type that agrees
  # on the observed nodes
  expect_true(sum(map["X0", ]) >= 1)
})

test_that("E matches nested apply under restrictions (drop and keep)", {
  drop_y00 <- set_restrictions(make_model("X -> Y"), labels = list(Y = "00"))
  expect_true(families_equal(drop_y00))
  map_drop <- CausalQueries:::get_data_families(drop_y00, mapping_only = TRUE)
  expect_true(all(c("X0Y0", "X1Y0", "X0Y1", "X1Y1") %in% colnames(map_drop)))

  keep_y00 <- set_restrictions(
    make_model("X -> Y"),
    labels = list(Y = "00"),
    keep = TRUE
  )
  expect_true(families_equal(keep_y00))
  map_keep <- CausalQueries:::get_data_families(keep_y00, mapping_only = TRUE)
  expect_identical(sort(colnames(map_keep)), c("X0Y0", "X1Y0"))
  # Impossible complete rows for this restriction are dropped from E rows
  # when drop_impossible is TRUE, but coarsened rows that still map remain
  expect_true("X0Y0" %in% rownames(map_keep))
  expect_false("X0Y1" %in% rownames(map_keep))

  # Keep Y.11 only: Y always 1 — Y=0 data types are impossible
  keep_y11 <- set_restrictions(
    make_model("X -> Y"),
    labels = list(Y = "11"),
    keep = TRUE
  )
  expect_true(families_equal(keep_y11))
  map_y11 <- CausalQueries:::get_data_families(keep_y11, mapping_only = TRUE)
  expect_identical(sort(colnames(map_y11)), c("X0Y1", "X1Y1"))
  expect_false(any(grepl("Y0", rownames(map_y11))))
  expect_true(all(c("X0Y1", "X1Y1") %in% rownames(map_y11)))
})

test_that("E matches nested apply with confounding", {
  model <- set_confound(make_model("X -> Y"), list("X <-> Y"))
  expect_true(families_equal(model))
})

test_that("prep_stan_data E agrees with get_data_families mapping (complete and partial)", {
  model <- make_model("X -> Y")
  fam <- CausalQueries:::get_data_families(model)

  d_complete <- collapse_data(make_data(model, n = 8), model)
  sd_c <- CausalQueries:::prep_stan_data(model, d_complete)
  expect_identical(
    unname(as.matrix(fam[d_complete$event, setdiff(names(fam), c("event", "strategy"))])),
    unname(sd_c$E)
  )
  expect_equal(ncol(sd_c$E), sd_c$n_data)
  expect_equal(nrow(sd_c$E), sd_c$n_events)

  d_part <- collapse_data(
    data.frame(X = c(0, 1, NA), Y = c(0, 0, 1)),
    model
  )
  sd_p <- CausalQueries:::prep_stan_data(model, d_part)
  expect_identical(
    unname(as.matrix(fam[d_part$event, setdiff(names(fam), c("event", "strategy"))])),
    unname(sd_p$E)
  )
  # Multi-strategy / missingness: more event rows than complete types
  expect_gt(sd_p$n_events, sd_p$n_data)
  expect_equal(ncol(sd_p$E), sd_p$n_data)
})
