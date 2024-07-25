# testthat-based unit tests for the functionality of the read_inv function
require(testthat)

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

  # class is maintained after rbind
  expect_true(
    inherits(rbind(test_inv, test_inv), "forest_inv")
  )

  # reading in works with explicitly specified names
  expect_no_error({
    test1 <- test
    names(test1) <- letters[1:4]
    test_inv1 <- read_inv(test1, id = "a", x = "b", y = "c",
                          dbh = "d", verbose = FALSE)
  })
  expect_equal(test_inv, test_inv1, ignore_attr = TRUE)

  # reading in with a subset of specified names
  expect_no_error({
    test_inv2 <- read_inv(test1, id = "a", x = "b", y = "c",
                          verbose = FALSE) # d is a valid alternative for dbh
  })
  expect_equal(test_inv, test_inv2, ignore_attr = TRUE)

  # reading in with a subset of specified names in unquoted form
  expect_no_error({
    test_inv3 <- read_inv(test1, id = a, x = b, y = c,
                          verbose = FALSE)
  })
  expect_equal(test_inv, test_inv3, ignore_attr = TRUE)

  # reading in works with different order of columns
  expect_no_error({
    test2 <- test[, c(3,1,4,2)]
    test_inv4 <- read_inv(test2, verbose = FALSE)
  })
  expect_equal(test_inv, test_inv4, ignore_attr = TRUE)

  # reading in works with size
  expect_no_error({
    test_inv5 <- read_inv(test, size = DBH, verbose = FALSE)
  })
  expect_equal(test_inv$dbh, test_inv5$size, ignore_attr = TRUE)


  # reading in fails with an error if class of a vector is incorrect
  expect_error({
    test3 <- test
    test3$DBH <- as.character(test3$DBH)
    read_inv(test3, verbose = FALSE)
  },
  "Variable identified as dbh"
  )

  # reading in fails with an error if a coordinate is missing
  expect_error({
    read_inv(test[, -2], verbose = FALSE)
  },
  "No variable found with a name"
  )

  # reading in fails with an error if forbidden IDs are used
  expect_error({
    test4 <- test
    test4$TreeID[c(2,3)] <- c("buff_edge", "all_trees")
    read_inv(test4, verbose = FALSE)
  },
  "Found the following tree ids:"
  )

  # reading in without diameter or height is possible
  expect_length(read_inv(test[, -4], verbose = FALSE), 3)

  # reading in works with extra columns
  expect_no_error({
    test5 <- test
    test5$extra <- 1:nrow(test5)

    # no parsing of extra column
    test_inv5 <- read_inv(test5, verbose = FALSE)
    # keeping the extra column
    test_inv6 <- read_inv(test5, keep_rest = TRUE, verbose = FALSE)
  })

  # extra column exists and has the right name
  expect_equal(
    names(test_inv6)[!names(test_inv6) %in% names(test_inv5)], "extra")

  # it is possible to create a forest_inv with different column types
  expect_length({
    test5$char <- "C"
    test5$log  <- TRUE
    test5$fact <- factor(test5$extra)
    test5$list <- as.list(test5$extra)
    test_inv7 <- read_inv(test5, keep_rest = TRUE, verbose = FALSE)
  }, 9)

  # printing the inventory with different column types is possible
  expect_output(print(test_inv7), "'forest_inv' class inventory dataset")

  # reading in works if there are no ids - but standard is assigned
  expect_message(read_inv(test[, -1]), "automatically generated")
})


test_that("Unit handling works", {
  # reading dbh in ms works
  expect_equal({
    test <- read.csv(test_path("testdata", "smallinv1.csv"),
                     sep = ",", dec = ".")
    # read as meter
    read_inv(test, dbh_unit = "m", verbose = FALSE)$dbh},
    # outcome should be 100 times higher (assumed to be converted to cm)
    100 * test$DBH, ignore_attr = TRUE)
  # reading dbh in mm works
  expect_equal(
    read_inv(test, dbh_unit = "mm", verbose = FALSE)$dbh,
    0.1 * test$DBH, #should be 0.1 of the original due to conversion to cm
    ignore_attr = TRUE)

  # reading height in m works
  expect_equal({
    test$height <- test$DBH
    read_inv(test, verbose = FALSE)$height},
    # outcome should be 100 times higher (assumed to be converted to cm)
    test$height, ignore_attr = TRUE)

  # reading height in cm works
  expect_equal({
    read_inv(test, height_unit = "cm", verbose = FALSE)$height},
    # outcome should be 100 times higher (assumed to be converted to cm)
    test$height * 0.01, ignore_attr = TRUE)

  # reading height in mm works
  expect_equal({
    read_inv(test, height_unit = "mm", verbose = FALSE)$height},
    # outcome should be 100 times higher (assumed to be converted to cm)
    test$height * 0.001, ignore_attr = TRUE)
})


test_that("read_inv works for file paths", {
  # standard csv - should read in with a message about
  # column specifications
  expect_message({
    f1 <- read_inv(test_path("testdata", "smallinv1.csv"))
  })

  # manually specify separators in German style csv with sep = ";" and dec = ","
  expect_equal(
    read_inv(test_path("testdata", "smallinv2.csv"),
             sep = ";", dec = ",", verbose = FALSE),
    f1)

  # tabstop delimited dataset
  expect_equal(
    read_inv(test_path("testdata", "smallinv3.txt"),
             verbose = FALSE),
    f1)
})


test_that("print method for forest_pc objects works", {
  # print is possible without error
  expect_output(
    print(read_inv(test_path("testdata", "smallinv1.csv"),
                   verbose = FALSE)),
    "'forest_inv' class inventory dataset"
  )
})




test_that("NA handling for forest_pc objects works", {
  # read as test ata
  test <- read.csv(test_path("testdata", "smallinv1.csv"),
                   sep = ",", dec = ".")

  # NA in x coordinates triggers error
  expect_error({
    tt <- test
    tt$X[1] <- NA
    read_inv(tt, verbose = FALSE)
  },
  "No missing values allowed"
  )

  # NA in y coordinates triggers error
  expect_error({
    tt <- test
    tt$Y[1] <- NA
    read_inv(tt, verbose = FALSE)
  },
  "No missing values allowed"
  )

  # NA in tree ID triggers error
  expect_error({
    tt <- test
    tt$TreeID[1] <- NA
    read_inv(tt, verbose = FALSE)
  },
  "No missing values allowed"
  )

  # NA in DBH triggers no error
  expect_no_error({
    tt <- test
    tt$DBH[1] <- NA
    read_inv(tt, verbose = FALSE)
  }
  )

  # NA in height triggers no error
  expect_no_error({
    tt <- test
    tt$height <- 1
    tt$height[1] <- NA
    read_inv(tt, verbose = FALSE)
  }
  )

  # NA in size triggers no error
  expect_no_error({
    tt <- test
    tt$size <- 1
    tt$size[1] <- NA
    read_inv(tt, verbose = FALSE)
  }
  )

  # NA in other variables triggers no error
  expect_no_error({
    tt <- test
    tt$brp <- 1
    tt$brp[1] <- NA
    read_inv(tt, verbose = FALSE, keep_rest = TRUE)
  }
  )
})

