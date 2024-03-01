test_that("reading a tree point cloud in txt format works", {
  # try if loading works without error
  expect_no_error({
    test_tree <-  read_tree(path = test_path("testdata", "tree.txt"))
  })

  # test if loaded object has the correct class
  expect_match(
    class(test_tree),
    "data.frame"
  )

  # test if loaded object has the correct dimensions
  expect_equal(
    dim(test_tree),
    c(255874, 3)
    )

  # test if loaded object has the correct column names
  expect_equal(
    names(test_tree),
    c("X", "Y", "Z")
  )

  # test if the loaded object has the correct values
  expect_equal(
    colSums(test_tree),
    c(X = 205070.26, Y = -132578.89, Z = 1995800.38)
    )
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
    compete_pc(forest_path = test_path("testdata", "neighborhood.txt"),  tree_path = test_path("testdata", "tree.txt"), comp_method = "cone")),
    2)
})

test_that("quantify competition (cone) for .txt file point clouds works", {
  expect_equal(length(
    compete_pc(forest_path = test_path("testdata", "neighborhood.txt"),  tree_path = test_path("testdata", "tree.txt"), comp_method = "cone")),
    2)
})

test_that("quantify competition (cone) for .las file tree and .txt file forest point clouds works", {
  expect_equal(length(
    compete_pc(forest_path = test_path("testdata", "neighborhood.txt"),  tree_path = test_path("testdata", "tree.las"), comp_method = "cone")),
    2)
})

test_that("wrong method - warning message", {
  expect_error(compete_pc(forest_path = test_path("testdata", "neighborhood.txt"),  tree_path = test_path("testdata", "tree.txt"), comp_method = "cyl", cyl_r = 4), "Invalid method. Use 'cone' or 'cylinder'.")
})

test_that("quantify competition (cylinder) for .txt file point clouds works with customized radius", {
  expect_equal(length(
    compete_pc(forest_path = test_path("testdata", "neighborhood.txt"),  tree_path = test_path("testdata", "tree.txt"), comp_method = "cylinder", cyl_r = 3)),
    2)
})


test_that("Hegyi index works", {
  expect_equal(length(
    compete_dd(plot_path = test_path("testdata", "inventory.csv"), ttrees_path = test_path("testdata", "targettrees_inventory.csv"), radius = 10, method = "CI_Hegyi",dbh_thr = 0.1)),
    2)
})

test_that("CI_RK1 index works", {
  expect_equal(length(
    compete_dd(plot_path = test_path("testdata", "inventory.csv"), ttrees_path = test_path("testdata", "targettrees_inventory.csv"), radius = 10, method = "CI_RK1",dbh_thr = 0.1)),
    2)
})

test_that("CI_RK2 index works", {
  expect_equal(length(
    compete_dd(plot_path = test_path("testdata", "inventory.csv"), ttrees_path = test_path("testdata", "targettrees_inventory.csv"), radius = 10, method = "CI_RK2",dbh_thr = 0.1)),
    2)
})


test_that("all indices at once work with compete_dd()", {
  expect_equal(length(
    compete_dd(plot_path = test_path("testdata", "inventory.csv"), ttrees_path = test_path("testdata", "targettrees_inventory.csv"), radius = 10, method = "all",dbh_thr = 0.1)),
    4)
})



test_that("all indices at once for ALS inventory", {
  expect_equal(ncol(
    compete_dh(plot_path = test_path("testdata", "inventory_ALS.csv"), ttrees_path = test_path("testdata", "targettrees_ALS.csv"), radius = 10, method = "all")),
    4)
})


test_that("Braathe index works", {
  expect_equal(ncol(
    compete_dh(plot_path = test_path("testdata", "inventory_ALS.csv"), ttrees_path = test_path("testdata", "targettrees_ALS.csv"), radius = 10, method = "CI_Braathe")),
    2)
})

test_that("RK3 index works", {
  expect_equal(ncol(
    compete_dh(plot_path = test_path("testdata", "inventory_ALS.csv"), ttrees_path = test_path("testdata", "targettrees_ALS.csv"), radius = 10, method = "CI_RK3")),
    2)
})

test_that("RK4 index works", {
  expect_equal(ncol(
    compete_dh(plot_path = test_path("testdata", "inventory_ALS.csv"), ttrees_path = test_path("testdata", "targettrees_ALS.csv"), radius = 10, method = "CI_RK4")),
    2)
})

