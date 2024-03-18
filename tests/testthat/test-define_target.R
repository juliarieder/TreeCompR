# testthat-based unit tests for the functionality of the read_inv function
test_that("define_target works for character vectors", {
  # reading in data
  expect_no_error({
    test_inv <- read_inv(test_path("testdata", "inventory.csv"),
                         sep = ";", dec = ".", verbose = FALSE)
  })

  # test if target can be defined for a single observations
  expect_no_error({
    test1 <- define_target(inv = test_inv,
                           target_source = "3")
  })
  # test if correct rows are returned
  expect_equal(test1$id[test1$target], "3")
  # test if type is passed on
  expect_equal(attr(test1, "target_type"), "character vector")


  # test if target can be defined for multiple observations
  expect_no_error({
     test2 <- define_target(inv = test_inv,
                            target_source = c("51", "52"))
  })
  # test if correct rows are returned
  expect_equal(test2$id[test2$target], c("51", "52"))
  # test if type is passed on
  expect_equal(attr(test2, "target_type"), "character vector")

  # test for warning if target does not exist
  expect_warning(
    define_target(inv = test_inv, target_source = "wrong ID"),
    "No target trees have been found"
  )
})


test_that("define_target works for logical vectors", {
  # test if target can be defined with logical vector
  expect_no_error({
    test_inv <- read_inv(test_path("testdata", "inventory.csv"),
                         sep = ";", dec = ".", verbose = FALSE)
    test3 <- define_target(inv = test_inv,
                           target_source = rep(c(TRUE, TRUE, FALSE), 16))
  })
  # test if correct rows are returned (only the rows not divisible by 3)
  expect_true(!any(as.numeric((1:nrow(test3))[test3$target]) %% 3 == 0))
  # test if type is passed on
  expect_equal(attr(test3, "target_type"), "logical vector")


  # test if wrong length results in error
  expect_error(
    define_target(inv = test_inv,
                  target_source = c(FALSE, FALSE, TRUE)),
    "If 'target_source' is a logical vector"
  )
})

test_that("define_target works for other inventories", {
  # test if target can be defined with subset
  expect_no_error({
    test_inv <- read_inv(test_path("testdata", "inventory.csv"),
                         sep = ";", dec = ".", verbose = FALSE)
    target <- test_inv[c(3, 5:17), ]
    test4 <- define_target(inv = test_inv, target_source = target)
  })
  # test if correct rows are returned
  expect_equal(test_inv$id[c(3, 5:17)], test4$id[test4$target])
  # test if type is passed on
  expect_equal(attr(test4, "target_type"), "second inventory")

  # test if it works when no dbh is available
  expect_no_error({
   define_target(inv = test_inv, target_source = target[, 1:3])
  })

  # test if it works if IDs are different
  expect_no_error({
    target1 <- target[1:2,]
    target1$id <- paste("target", target1$id)
    test5 <- define_target(inv = test_inv, target_source = target1)
  })
  # test if correct IDs are passed on
  expect_equal(test5$target_id[test5$target], c("target 3", "target 5"))

  # test if it works if there are duplicate IDs
  expect_error({
    target2 <- rbind(target1, target1)
    target2$id <- letters[1:4]
    define_target(inv = test_inv, target_source = target2)
  }, "More than one point is any equally good match.")
})



test_that("define_target works for methods specified as character strings", {
  # test if target can be defined with subset
  expect_warning({
    test_inv <- read_inv(test_path("testdata", "inventory.csv"),
                         sep = ";", dec = ".", verbose = FALSE)
    test6 <- define_target(inv = test_inv, target_source = "all")
    },
    "Defining all trees as target trees is rarely a good idea.")
  # test if type is passed on
  expect_equal(attr(test6, "target_type"), "all trees")

  # test if missing radius results in error for spatial methods
  expect_error({
    define_target(inv = test_inv, target_source = "exclude_edge")
  }, "'radius' is required for methods 'buff_edge' and 'exclude_edge'.")
  expect_error({
    define_target(inv = test_inv, target_source = "buff_edge")
  }, "'radius' is required for methods 'buff_edge' and 'exclude_edge'.")

  # test if target can be defined by removing only edge trees
  expect_no_error({
    test7 <- define_target(inv = test_inv, target_source = "exclude_edge",
                           radius = 10)
  })
  # test if type is passed on
  expect_equal(attr(test7, "target_type"), "excluding edge")

  # test if target can be defined by removing edge trees with a buffer around
  # the plot margin
  expect_no_error({
    test8 <- define_target(inv = test_inv,
                           target_source = "buff_edge",
                           radius = 10)
  })
  # test if type is passed on
  expect_equal(attr(test8, "target_type"), "excluding buffer around edge")

  # test if print methods works
  expect_output(print(test8), "'target_inv' class inventory dataset")
})


