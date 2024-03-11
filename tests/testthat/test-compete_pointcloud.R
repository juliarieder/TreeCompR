# testthat-based unit tests for the functionality of the compete_pc function

test_that("compete_pc works for .txt file point clouds", {
  # test basic functionality for cone method
  expect_equal(
    length(
      compete_pc(forest_source = test_path("testdata", "neighborhood.txt"),
                 tree_source = test_path("testdata", "tree.txt"),
                 comp_method = "cone",
                 print_progress = "none")
    ),
    5)

  # test basic functionality for cylinder method
  expect_equal(
    length(
      compete_pc(forest_source = test_path("testdata", "neighborhood.txt"),
                 tree_source = test_path("testdata", "tree.txt"),
                 comp_method = "cylinder",
                 print_progress = "none")
    ),
    5)

  # test basic functionality for output of both methods
  expect_equal(
    length(
      compete_pc(forest_source = test_path("testdata", "neighborhood.txt"),
                 tree_source = test_path("testdata", "tree.txt"),
                 comp_method = "both",
                 print_progress = "none")
    ),
    7)
})


test_that("compete_pc works for .las file tree and .txt file forest point clouds", {
  expect_equal(
    length(
      compete_pc(forest_source = test_path("testdata", "neighborhood.txt"),
                 tree_source = test_path("testdata", "tree.las"),
                 comp_method = "cone",
                 print_progress = "none")
    ),
    5)
})

test_that("Wrong method in compete_pc fails with an error message", {
  expect_error(
    compete_pc(forest_source = test_path("testdata", "neighborhood.txt"),
               tree_source = test_path("testdata", "tree.txt"),
               comp_method = "clyinder", cyl_r = 4,
               print_progress = "none")
  )
})

test_that("compete_pc works for .txt file point clouds with customized radius", {
  expect_equal(
    length(
      compete_pc(forest_source = test_path("testdata", "neighborhood.txt"),
                 tree_source = test_path("testdata", "tree.txt"),
                 comp_method = "cylinder", cyl_r = 3,
                 print_progress = "none")
    ),
    5)
})
