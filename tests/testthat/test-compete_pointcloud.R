# testthat-based unit tests for the functionality of the compete_pc function

test_that("compete_pc works for .txt file point clouds", {
  # test basic functionality for cone method
  expect_no_error(
    test1 <- compete_pc(
      forest_source = test_path("testdata", "neighborhood.txt"),
      tree_source = test_path("testdata", "tree.txt"),
      comp_method = "cone",
      print_progress = "none")
  )

  # test basic functionality for cylinder method
  expect_no_error(
    test2 <- compete_pc(
      forest_source = test_path("testdata", "neighborhood.txt"),
      tree_source = test_path("testdata", "tree.txt"),
      comp_method = "cylinder", center_position = "base_pos",
      print_progress = "none")
  )

  # test basic functionality for output of both methods
  expect_no_error(
    test3 <- compete_pc(
      forest_source = test_path("testdata", "neighborhood.txt"),
      tree_source = test_path("testdata", "tree.txt"),
      comp_method = "both", center_position = "base_pos",
      print_progress = "none")
  )

  # test basic functionality for output of both methods when calculated with a
  # custom voxel resolution
  expect_warning(
    test4 <- compete_pc(
      forest_source = test_path("testdata", "neighborhood.txt"),
      tree_source = test_path("testdata", "tree.txt"),
      comp_method = "both", center_position = "base_pos",
      res = 1,
      print_progress = "none"),
    "Creating voxelized dataset"
  )

  # test for dimensions
  expect_equal(
    c(length(test1), length(test2),
      length(test3), length(test4)),
    c(5,5,7,7))

  # test if center position is correct
  expect_equal(test1$center_position, "crown center")
  expect_equal(test2$center_position, "base")
  expect_equal(test3$center_position, "base")
  expect_equal(test4$center_position, "base")

  # test if different values are returned if different bases are used
  expect_false(test1$CI_cone == test3$CI_cone)
  # test if the same values are returned for the same base
  expect_true(test2$CI_cyl == test3$CI_cyl)

  # test if a larger voxel size results in smaller values
  expect_true(all(test3[,c(4,6)] > test4[,c(4,6)]))
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

test_that("compete_pc works for .ply file tree and .txt file forest point clouds", {
  expect_equal(
    length(
      compete_pc(forest_source = test_path("testdata", "neighborhood.txt"),
                 tree_source = test_path("testdata", "tree.ply"),
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
  # loading works
  expect_no_error({
    test <- compete_pc(forest_source = test_path("testdata", "neighborhood.txt"),
                       tree_source = test_path("testdata", "tree.txt"),
                       comp_method = "cylinder", cyl_r = 3,
                       print_progress = "none")
  })

  # Output is printed correctly
  expect_output(
    print(test),
    "Point cloud based competition indices for 'tree'"
  )
  # no specified center position -> crown center
  expect_equal(test$center_position, "crown center")

  # correct radius returned
  expect_equal(test$cyl_r, 3)
})

