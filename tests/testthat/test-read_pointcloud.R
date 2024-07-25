# testthat-based unit tests for the functionality of the read_pc function

test_that("reading a tree point cloud in txt format works", {
  # try if loading works without error
  expect_message({ # message expected due to unnamed dataset
    test_tree <-  read_pc(pc_source = test_path("testdata", "tree.txt"))
  })

  # test if loaded object has the correct class
  expect_true(
    inherits(test_tree, "forest_pc")
  )

  # class is maintained after rbind
  expect_true(
    inherits(rbind(test_tree[1:3,], test_tree[1:3,]), "forest_pc")
  )


  # test if loaded object has the correct dimensions
  expect_equal(
    dim(test_tree),
    c(200389 , 3)
  )

  # test if loaded object has the correct column names
  expect_equal(
    names(test_tree),
    c("x", "y", "z")
  )

  # test if the loaded object has the correct values
  expect_equal(
    colSums(test_tree),
    c(x = 196801.95, y = -108219.71, z = 1868380.0)
  )

  # test if base position works
  expect_equal(
    {pos <- tree_pos(test_tree)
    pos$base_pos},
    c(x = 0.1, y = -0.5, z = -0.1)
  )
})


test_that("reading neighborhood in txt format works", {
  # try if loading works without error
  expect_message({ # message expected due to unnamed dataset
    test_neigh <-  read_pc(pc_source = test_path("testdata", "neighborhood.txt"))
  })

  # test if loaded object has the correct class
  expect_true(
    inherits(test_neigh, "forest_pc")
  )

  # test if loaded object has the correct dimensions
  expect_equal(
    dim(test_neigh),
    c(4019017, 3)
  )

  # test if loaded object has the correct column names
  expect_equal(
    names(test_neigh),
    c("x", "y", "z")
  )

  # test if the loaded object has the correct values
  expect_equal(
    colSums(test_neigh),
    c(x = 7886948.89, y = -4657071.24, z = 13619662.60)
  )
})


test_that("reading pc in las format works", {
  # try if loading works without error
  expect_no_error({
    test_tree <-  read_pc(pc_source = test_path("testdata", "tree.las"))
  })

  # same output when loading outside of TreeCompR
  expect_equal({
    test <- lidR::readTLSLAS(test_path("testdata", "tree.las"))
    read_pc(test)
  }, test_tree)


  # test if loaded object has the correct class
  expect_true(
    inherits(test_tree, "forest_pc")
  )

  # test if loaded object has the correct dimensions
  expect_equal(
    dim(test_tree),
    c(200389, 3)
  )

  # test if loaded object has the correct column names
  expect_equal(
    names(test_tree),
    c("x", "y", "z")
  )

  # test if the loaded object has the correct values
  expect_equal(
    colSums(test_tree),
    c(x = 196801.95, y = -108219.71, z = 1868380.0)
  )
})


test_that("reading pc in laz format works", {
  # try if loading works without error
  expect_no_error({
    test_tree <-  read_pc(pc_source = test_path("testdata", "tree.laz"))
  })

  # test if loaded object has the correct class
  expect_true(
    inherits(test_tree, "forest_pc")
  )

  # test if loaded object has the correct dimensions
  expect_equal(
    dim(test_tree),
    c(200389, 3)
  )

  # test if loaded object has the correct column names
  expect_equal(
    names(test_tree),
    c("x", "y", "z")
  )

  # test if the loaded object has the correct values
  expect_equal(
    colSums(test_tree),
    c(x = 196801.95, y = -108219.71, z = 1868380.0)
    )
})


test_that("reading pc in ply format works", {
  # try if loading works without error
  expect_no_error({
    test_tree <-  read_pc(pc_source = test_path("testdata", "tree.ply"))
  })

  # test if loaded object has the correct class
  expect_true(
    inherits(test_tree, "forest_pc")
  )

  # test if loaded object has the correct dimensions
  expect_equal(
    dim(test_tree),
    c(200389, 3)
  )

  # test if loaded object has the correct column names
  expect_equal(
    names(test_tree),
    c("x", "y", "z")
  )

  # test if the loaded object has the correct values
  expect_equal(
    colSums(test_tree),
    c(x = 196801.95, y = -108219.71, z = 1868380.0)
    )
})

test_that("reading point clouds with filters works", {
  # try if loading works without error
  expect_no_error({
    test_tree <-  read_pc(pc_source = test_path("testdata", "tree.las"),
                          xlim = c(0,Inf), ylim = c(0, Inf), zlim = c(0, 17))
  })

  # test if values are in the specified bounds
  expect_gte(min(test_tree$x), 0)
  expect_gte(min(test_tree$y), 0)
  expect_gte(min(test_tree$z), 0)
  expect_lte(max(test_tree$z), 17)
})


test_that(
  "tabular pcs w/ different types, structures and extensions can be read", {
    # simple csv with named xyz columns is read without message
    expect_no_message({
      tinytree1 <- read_pc(test_path("testdata", "tinytree1.csv"))
    })

    # csv with different field and decimal separator can be read
    expect_equal(
      tinytree1,
      read_pc(test_path("testdata", "tinytree2.csv"),
              dec = ",", sep = ";")
    )

    # csv with different field and decimal separator and a first column of class
    # character can be read
    expect_equal(
      tinytree1,
      read_pc(test_path("testdata", "tinytree3.csv"),
              dec = ",", sep = ";")
    )

    # tabstop delimited txt with d a first column of class character and wrongly
    # labeled columns can be read (resulting in a message)
    expect_message({
      tinytree4 <- read_pc(test_path("testdata", "tinytree4.txt"),
                           sep = "\t")
    })
    expect_equal(tinytree1, tinytree4)

    # messages are correctly suppressed with verbose = FALSE
    expect_no_message({
      read_pc(test_path("testdata", "tinytree4.txt"),
              sep = "\t", verbose = FALSE)
    })

    # standard csv with different order (x, z, y) is read correctly
    expect_equal(
      tinytree1,
      read_pc(test_path("testdata", "tinytree5.csv"))
    )

    # trees can be loaded and validated if provided as data.frames
    expect_equal(
      tinytree1,{
        tb <- read.table(
          test_path("testdata", "tinytree4.txt"),
          header = TRUE)
        read_pc(tb, verbose = FALSE)}
    )

    # an error is thrown if columns are missing
    expect_error(
      read_pc(as.data.frame(tinytree1[,-1])),
      regexp = "Point cloud dataset contains less than 3"
    )

    # an error is thrown if the data are characters
    expect_error({
      test <- as.data.frame(tinytree1)
      test$x <- as.character(test$x)
      read_pc(test)},
      regexp = "One or more of the coordinate vectors x, y, z is not numeric"
    )
  })

test_that("print method for forest_pc objects works", {
  # simple csv with named xyz columns is read without message
  expect_output(
    print(read_pc(test_path("testdata", "tinytree1.csv"))),
    "'forest_pc' class point cloud"
  )
})


