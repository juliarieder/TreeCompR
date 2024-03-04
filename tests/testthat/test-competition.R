test_that("reading a tree point cloud in txt format works", {
  # try if loading works without error
  expect_message({ # message expected due to unnamed dataset
    test_tree <-  read_tree(tree_source = test_path("testdata", "tree.txt"))
  })

  # test if loaded object has the correct class
  expect_true(
    inherits(test_tree, "tree_pc")
  )

  # test if loaded object has the correct dimensions
  expect_equal(
    dim(test_tree),
    c(255874, 3)
  )

  # test if loaded object has the correct column names
  expect_equal(
    names(test_tree),
    c("x", "y", "z")
  )

  # test if the loaded object has the correct values
  expect_equal(
    colSums(test_tree),
    c(x = 205070.26, y = -132578.89, z = 1995800.38)
  )

  # test if base position works
  expect_equal(
    {pos <- tree_pos(test_tree)
    pos$base_pos},
    c(x = 0.1, y = -0.5, z = -0.1)
  )

  # test if crown position works
  expect_equal(
    pos$crown_pos,
    c(x = 1.8, y = -0.61, z = -0.1)
  )
  # test if height works
  expect_equal(
    tree_pos(test_tree)$height,
    22.8
  )
})



test_that("reading a neighborhood point cloud in txt format works", {
  # try if loading works without error
  expect_message({ # message expected due to unnamed dataset
    test_tree <-  read_tree(tree_source = test_path("testdata", "neighborhood.txt"))
  })

  # test if loaded object has the correct class
  expect_true(
    inherits(test_tree, "tree_pc")
  )

  # test if loaded object has the correct dimensions
  expect_equal(
    dim(test_tree),
    c(4908829, 3)
  )

  # test if loaded object has the correct column names
  expect_equal(
    names(test_tree),
    c("x", "y", "z")
  )

  # test if the loaded object has the correct values
  expect_equal(
    colSums(test_tree),
    c(x = 9139977.58, y = -5456060.20, z = 14519802.21)
  )
})


test_that("reading a tree point cloud in las format works", {
  # try if loading works without error
  expect_no_error({
    test_tree <-  read_tree(tree_source = test_path("testdata", "tree.las"))
  })

  # test if loaded object has the correct class
  expect_true(
    inherits(test_tree, "tree_pc")
  )

  # test if loaded object has the correct dimensions
  expect_equal(
    dim(test_tree),
    c(255874, 3)
  )

  # test if loaded object has the correct column names
  expect_equal(
    names(test_tree),
    c("x", "y", "z")
  )

  # test if the loaded object has the correct values
  expect_equal(
    colSums(test_tree),
    c(x = 205070.26, y = -132578.89, z = 1995800.38)
  )
})


test_that("reading a tree point cloud in laz format works", {
  # try if loading works without error
  expect_no_error({
    test_tree <-  read_tree(tree_source = test_path("testdata", "tree.laz"))
  })

  # test if loaded object has the correct class
  expect_true(
    inherits(test_tree, "tree_pc")
  )

  # test if loaded object has the correct dimensions
  expect_equal(
    dim(test_tree),
    c(255874, 3)
  )

  # test if loaded object has the correct column names
  expect_equal(
    names(test_tree),
    c("x", "y", "z")
  )

  # test if the loaded object has the correct values
  expect_equal(
    colSums(test_tree),
    c(x = 205070.26, y = -132578.89, z = 1995800.38)
  )


})

test_that("tabular point clouds with different types, structures and extensions can be read", {
  # simple csv with named xyz columns is read without message
  expect_no_message({
    tinytree1 <- read_tree(test_path("testdata", "tinytree1.csv"))
  })

  # csv with different field and decimal separator can be read
  expect_equal(
    tinytree1,
    read_tree(test_path("testdata", "tinytree2.csv"),
              dec = ",", sep = ";")
  )

  # csv with different field and decimal separator and a first column of class
  # character can be read
  expect_equal(
    tinytree1,
    read_tree(test_path("testdata", "tinytree3.csv"),
              dec = ",", sep = ";")
  )

  # tabstop delimited txt with d a first column of class character and wrongly
  # labeled columns can be read (resulting in a message)
  expect_message({
    tinytree4 <- read_tree(test_path("testdata", "tinytree4.txt"),
                           sep = "\t")
  })
  expect_equal(tinytree1, tinytree4)

  # standard csv with different order (x, z, y) is read correctly
  expect_equal(
    tinytree1,
    read_tree(test_path("testdata", "tinytree5.csv"))
  )

  # trees can be loaded and validated if provided as data.frames
  expect_equal(
    tinytree1,
    read.table(
      test_path("testdata", "tinytree4.txt"),
      header = TRUE) %>%
      read_tree()
  )
})

test_that("compete_pc works for .txt file point clouds", {
  # test basic functionality for cone method
  expect_equal(
    length(
      compete_pc(forest_source = test_path("testdata", "neighborhood.txt"),
                 tree_source = test_path("testdata", "tree.txt"),
                 comp_method = "cone")
    ),
    5)

  # test basic functionality for cylinder method
  expect_equal(
    length(
      compete_pc(forest_source = test_path("testdata", "neighborhood.txt"),
                 tree_source = test_path("testdata", "tree.txt"),
                 comp_method = "cylinder")
    ),
    5)

  # test basic functionality for output of both methods
  expect_equal(
    length(
      compete_pc(forest_source = test_path("testdata", "neighborhood.txt"),
                 tree_source = test_path("testdata", "tree.txt"),
                 comp_method = "both")
    ),
    7)
})


test_that("compete_pc works for .las file tree and .txt file forest point clouds", {
  expect_equal(
    length(
      compete_pc(forest_source = test_path("testdata", "neighborhood.txt"),
                 tree_source = test_path("testdata", "tree.las"), comp_method = "cone"
      )),
    5)
})

test_that("Wrong method in compete_pc fails with an error message", {
  expect_error(
    compete_pc(forest_source = test_path("testdata", "neighborhood.txt"),
               tree_source = test_path("testdata", "tree.txt"),
               comp_method = "clyinder", cyl_r = 4)
    )
})

test_that("compe_pc works for .txt file point clouds with customized radius", {
  expect_equal(
    length(
      compete_pc(forest_source = test_path("testdata", "neighborhood.txt"),
                 tree_source = test_path("testdata", "tree.txt"), comp_method = "cylinder", cyl_r = 3)
    ),
    5)
})

test_that("Hegyi index works", {
  expect_equal(
    length(
      compete_dd(plot_path = test_path("testdata", "inventory.csv"),
                 ttrees_path = test_path("testdata", "targettrees_inventory.csv"),
                 radius = 10, method = "CI_Hegyi",dbh_thr = 0.1)
    ),
    2)
})

test_that("CI_RK1 index works", {
  expect_equal(
    length(
      compete_dd(plot_path = test_path("testdata", "inventory.csv"),
                 ttrees_path = test_path("testdata", "targettrees_inventory.csv"),
                 radius = 10, method = "CI_RK1",dbh_thr = 0.1)),
    2)
})

test_that("CI_RK2 index works", {
  expect_equal(
    length(
      compete_dd(plot_path = test_path("testdata", "inventory.csv"),
                 ttrees_path = test_path("testdata", "targettrees_inventory.csv"),
                 radius = 10, method = "CI_RK2",dbh_thr = 0.1)
    ),
    2)
})


test_that("all indices at once work with compete_dd()", {
  expect_equal(
    length(
      compete_dd(plot_path = test_path("testdata", "inventory.csv"),
                 ttrees_path = test_path("testdata", "targettrees_inventory.csv"),
                 radius = 10, method = "all",dbh_thr = 0.1)
    ),
    4)
})



test_that("all indices at once for ALS inventory", {
  expect_equal(
    ncol(
      compete_dh(plot_path = test_path("testdata", "inventory_ALS.csv"),
                 ttrees_path = test_path("testdata", "targettrees_ALS.csv"),
                 radius = 10, method = "all")
    ),
    4)
})


test_that("Braathe index works", {
  expect_equal(
    ncol(
      compete_dh(plot_path = test_path("testdata", "inventory_ALS.csv"),
                 ttrees_path = test_path("testdata", "targettrees_ALS.csv"),
                 radius = 10, method = "CI_Braathe")
    ),
    2)
})

test_that("RK3 index works", {
  expect_equal(
    ncol(
      compete_dh(plot_path = test_path("testdata", "inventory_ALS.csv"),
                 ttrees_path = test_path("testdata", "targettrees_ALS.csv"),
                 radius = 10, method = "CI_RK3")
    ),
    2)
})

test_that("RK4 index works", {
  expect_equal(
    ncol(
      compete_dh(plot_path = test_path("testdata", "inventory_ALS.csv"),
                 ttrees_path = test_path("testdata", "targettrees_ALS.csv"),
                 radius = 10, method = "CI_RK4")
    ),
    2)
})

