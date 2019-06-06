# library(gbiqq)
# context("Collapse Ambiguity Matrices")
#
# test_dag_canonical <-
# 	gbiqq::make_dag(add_edges(parent = "X",children = c("K", "Y")),
# 									add_edges(parent = c("K"),children = "Y"))
#
# test_dag_simple <-
# 	gbiqq::make_dag(add_edges(parent = "X",children = "Y"),
# 									add_edges(parent = "K",children = "Y"))
#
#
# test_dag_complex <-
# 	gbiqq::make_dag(add_edges(parent = "X",children = c("M","K","Y")),
# 									add_edges(parent = "Z",children = "Y2"),
# 									add_edges(parent = c("K","M"),children = "Y"))
#
#
# test_that("Ambiguity matrices can be collapsed in canonical example", {
# 	expect_length(object = gbiqq::collapse_ambiguity_matrices(test_dag_canonical),
# 								n = 2)
#
# 	expect_equal(dim(gbiqq::collapse_ambiguity_matrices(test_dag_canonical)[["Y_K"]]), c(8,64))
# })
#
# test_that("Ambiguity matrices can be collapsed with one endogenous variable", {
# 	expect_length(object = gbiqq::collapse_ambiguity_matrices(test_dag_simple),
# 								n = 3)
#
# 	expect_equal(dim(gbiqq::collapse_ambiguity_matrices(test_dag_simple)[["Y"]]), c(8,16))
# })
#
# test_that("Ambiguity matrices can be collapsed with fairly complex example", {
# 	expect_length(object = gbiqq::collapse_ambiguity_matrices(test_dag_complex),
# 								n = 4)
#
# 	expect_equal(dim(gbiqq::collapse_ambiguity_matrices(test_dag_complex)[["Y2"]]), c(4,4))
# 	expect_equal(dim(gbiqq::collapse_ambiguity_matrices(test_dag_complex)[["Y_K_M"]]), c(16,4096))
# })
