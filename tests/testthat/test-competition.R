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
