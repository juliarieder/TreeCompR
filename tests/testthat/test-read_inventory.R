# testthat-based unit tests for the functionality of the read_inv function

test_that("read_inv works for data.frame objects", {
  # reading in works
  expect_no_error({
    test <- read.csv(
      test_path("testdata", "inventory.csv"),
      sep = ";", dec = "."
    )
    test_inv <- read_inv(test, verbose = FALSE)
  })

  # output has the correct class and structure
  expect_true(inherits(test_inv, "forest_inv"))
  expect_equal(dim(test_inv), c(48, 4))
  expect_equal(names(test_inv), c("id", "x", "y", "dbh"))

  # reading in works with explicitly specified names
  expect_no_error({
    test1 <- test
    names(test1) <- letters[1:4]
    test_inv1 <- read_inv(test1, id = "a", x = "b", y = "c",
                          dbh = "d", verbose = FALSE)
  })
  expect_equal(test_inv, test_inv1)

  # reading in with a subset of specified names
  expect_no_error({
    test_inv2 <- read_inv(test1, id = "a", x = "b", y = "c",
                          verbose = FALSE) # d is a valid alternative for dbh
  })
  expect_equal(test_inv, test_inv2)

  # reading in with a subset of specified names in unquoted form
  expect_no_error({
    test_inv3 <- read_inv(test1, id = a, x = b, y = c,
                          verbose = FALSE)
  })
  expect_equal(test_inv, test_inv3)

  # reading in works with different order of columns
  expect_no_error({
    test2 <- test[, c(3,1,4,2)]
    test_inv4 <- read_inv(test2, verbose = FALSE)
  })
  expect_equal(test_inv, test_inv4)

  # reading in fails with an error if class of a vector is incorrect
  expect_error({
    test3 <- test
    test3$DBH <- as.character(test3$DBH)
    read_inv(test3, verbose = FALSE)
  },
  "Variable identified as dbh"
  )
})


test_that("read_inv works for file paths", {
  # shopuld read in with a message about column specifications
  expect_message({
    read_inv(test_path("testdata", "inventory.csv"))
  })
})
