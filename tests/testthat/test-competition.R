test_that("reading a tree point cloud in txt format works", {
  expect_equal(length(
    read_tree(path = test_path("testdata", "tree.txt"))),
    3)
})

test_that("reading a forest point cloud in txt format works", {
  expect_equal(length(
    read_tree(path = test_path("testdata", "neighborhood.txt"))),
    3)
})

test_that("reading a tree point cloud in las format works", {
  expect_equal(length(
    read_tree(path = test_path("testdata", "tree.las"))),
    3)
})

test_that("reading a tree point cloud in laz format works", {
  expect_equal(length(
    read_tree(path = test_path("testdata", "tree.laz"))),
    3)
})

test_that("quantify competition (cone) for .txt file point clouds works", {
  expect_equal(length(
    competition_pc(forest_path = test_path("testdata", "neighborhood.txt"),  tree_path = test_path("testdata", "tree.txt"), comp_method = "cone")),
    2)
})

test_that("quantify competition (cone) for .txt file point clouds works", {
  expect_equal(length(
    competition_pc(forest_path = test_path("testdata", "neighborhood.txt"),  tree_path = test_path("testdata", "tree.txt"), comp_method = "cone")),
    2)
})

test_that("quantify competition (cone) for .las file tree and .txt file forest point clouds works", {
  expect_equal(length(
    competition_pc(forest_path = test_path("testdata", "neighborhood.txt"),  tree_path = test_path("testdata", "tree.las"), comp_method = "cone")),
    2)
})

test_that("wrong method - warning message", {
  expect_error(competition_pc(forest_path = test_path("testdata", "neighborhood.txt"),  tree_path = test_path("testdata", "tree.txt"), comp_method = "cyl", cyl_r = 4), "Invalid method. Use 'cone' or 'cylinder'.")
})

test_that("quantify competition (cylinder) for .txt file point clouds works with customized radius", {
  expect_equal(length(
    competition_pc(forest_path = test_path("testdata", "neighborhood.txt"),  tree_path = test_path("testdata", "tree.txt"), comp_method = "cylinder", cyl_r = 3)),
    2)
})

#compete_calc <- function(path, radius = 10, dbh_thr = 0.1, target_tree, type = c("ID", "coordinates"), tolerance = 0.1, method = c("all", "Hegyi", "CI12", "CI13")
test_that("Hegyi index works with target tree ID", {
  expect_equal(length(
    compete_dd(path = test_path("testdata", "inventory.csv"),  radius = 10, dbh_thr = 0.1, target_tree = 14, type = "ID", method = "Hegyi")),
    2)
})

test_that("CI12 index works with target tree ID", {
  expect_equal(length(
    compete_dd(path = test_path("testdata", "inventory.csv"),  radius = 10, dbh_thr = 0.1, target_tree = 14, type = "ID", method = "CI_hd2")),
    2)
})

test_that("Hegyi index works with coordinates", {
  expect_equal(length(
    compete_dd(path = test_path("testdata", "inventory.csv"),  radius = 10, dbh_thr = 0.1, target_tree = c(0.2581, -0.4883), type = "coordinates", tolerance = 1, method = "Hegyi")),
    2)
})

test_that("all indices at once work with coordinates", {
  expect_equal(length(
    compete_dd(path = test_path("testdata", "inventory.csv"),  radius = 10, dbh_thr = 0.1, target_tree = c(0.2581, -0.4883), type = "coordinates", tolerance = 1, method = "all")),
    7)
})

test_that("all indices at once work with target ID", {
  expect_equal(length(
    compete_dd(path = test_path("testdata", "inventory.csv"),  radius = 10, dbh_thr = 0.1, target_tree = 14, type = "ID", method = "all")),
    7)
})

test_that("all indices at once for ALS inventory", {
  expect_equal(ncol(
    compete_dh(seg_path = test_path("testdata", "inventory_ALS.csv"), tree_path = test_path("testdata", "targettrees_ALS.csv"), radius = 10)),
    4)
})


test_that("all indices at once for list of target trees from inventory table", {
  expect_equal(ncol(
    compete_inv(seg_path = test_path("testdata", "inventory.csv"), tree_path = test_path("testdata", "targettrees_inventory.csv"), radius = 10, method = "all")),
    7)
})

