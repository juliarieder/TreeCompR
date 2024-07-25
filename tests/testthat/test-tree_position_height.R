# testthat-based unit tests for the functionality of the tree_pos function

test_that("reading a tree point cloud in txt format works", {
  # try if loading works without error
  expect_no_error({
    test_tree <-  read_pc(pc_source = test_path("testdata", "tree.txt"),
                          verbose = FALSE)
  })

  # test if base position works
  expect_equal(
    {pos <- tree_pos(test_tree)
    pos$base_pos},
    c(x = 0.1, y = -0.5, z = -0.1)
  )

  # test if crown position works
  expect_equal(
    pos$crown_pos,
    c(x = 1.9, y = -0.5, z = -0.1)
  )

  # test if tree height works
  expect_equal(
    tree_pos(test_tree)$height,
    22.8
  )
})
