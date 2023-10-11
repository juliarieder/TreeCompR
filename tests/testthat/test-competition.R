test_that("quantify competition (cone) for .txt file point clouds works", {
  expect_equal(length(
    competition_pc(forest_path = "../data/neighborhood.txt",  tree_path = "../data/tree.txt", comp_method = "cone")),
    3)
})

test_that("quantify competition (cylinder) for .txt file point clouds works", {
  expect_equal(length(
    competition_pc(forest_path = "../data/neighborhood.txt",  tree_path = "../data/tree.txt", comp_method = "cylinder", cyl_r = 4)),
    3)
})

test_that("quantify competition (cylinder) for .txt file point clouds works with customized radius", {
  expect_equal(length(
    competition_pc(forest_path = "../data/neighborhood.txt",  tree_path = "../data/tree.txt", comp_method = "cylinder", cyl_r = 3)),
    3)
})

#compete_calc <- function(path, radius = 10, dbh_thr = 0.1, target_tree, type = c("ID", "coordinates"), tolerance = 0.1, method = c("all", "Hegyi", "CI12", "CI13")
test_that("Hegyi index works with target tree ID", {
  expect_equal(length(
    compete_calc(path = "../data/inventory.csv",  radius = 10, dbh_thr = 0.1, target_tree = 14, type = "ID", method = "Hegyi")),
    2)
})

test_that("Hegyi index works with coordinates", {
  expect_equal(length(
    compete_calc(path = "../data/inventory.csv",  radius = 10, dbh_thr = 0.1, target_tree = c(0.2581, -0.4883), type = "coordinates", tolerance = 1, method = "Hegyi")),
    2)
})
