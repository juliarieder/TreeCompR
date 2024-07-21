# testthat-based unit tests for the functionality of the read_inv function
test_that(
  "character vectors are handled correctly", {
    # reading in data
    expect_no_error({
      test_inv <- read_inv(test_path("testdata", "inventory.csv"),
                           sep = ";", dec = ".", verbose = FALSE)
    })

    # target can be defined for a single observations
    expect_no_error({
      test1 <- define_target(inv = test_inv,
                             target_source = "3")
    })
    # correct rows are returned
    expect_equal(test1$id[test1$target], "3")
    # type is passed on
    expect_equal(attr(test1, "target_type"), "character")

    # class is maintained after rbind
    expect_true(
      inherits(rbind(test1[1:3,], test1[1:3,]), "target_inv")
    )

    # target can be defined for multiple observations
    expect_no_error({
      test2 <- define_target(inv = test_inv,
                             target_source = c("51", "52"))
    })
    # correct rows are returned
    expect_equal(test2$id[test2$target], c("51", "52"))
    # type is passed on
    expect_equal(attr(test2, "target_type"), "character")

    # warning is printed if no target exists
    expect_warning(
      define_target(inv = test_inv, target_source = "wrong ID"),
      "No target trees have been found"
    )

    # different warning is printed if some targets do not exist
    expect_warning(
      define_target(inv = test_inv,
                    target_source = c( "5","7","wrong ID","wronger ID")),
      "The following target tree id"
    )

    # crop_to_target results in a message
    expect_message({
      test3 <- define_target(inv = test_inv, target_source = c("51", "52"),
                             crop_to_target = TRUE)
    }, "outside the competitive zone")

    # less trees are returned when specifying crop_to_target
    expect_lt(nrow(test3), nrow(test2))

    # plotting with plot_target works
    expect_no_error(plot_target(test2, radius = 5))
  })


test_that(
  "logical vectors are handled correctly", {
    # target can be defined with logical vector
    expect_no_error({
      test_inv <- read_inv(test_path("testdata", "inventory.csv"),
                           sep = ";", dec = ".", verbose = FALSE)
      test3 <- define_target(inv = test_inv,
                             target_source = rep(c(TRUE, TRUE, FALSE), 16))
    })
    # correct rows are returned (only the rows not divisible by 3)
    expect_true(!any(as.numeric((1:nrow(test3))[test3$target]) %% 3 == 0))
    # type is passed on
    expect_equal(attr(test3, "target_type"), "logical")


    # wrong length results in error
    expect_error(
      define_target(inv = test_inv,
                    target_source = c(FALSE, FALSE, TRUE)),
      "If 'target_source' is a logical vector"
    )

    # plotting with plot_target works
    expect_no_error(plot_target(test3, radius = 5))
  })

test_that(
  "other inventories are handled correctly", {
    # target can be defined with subset
    expect_no_error({
      test_inv <- read_inv(test_path("testdata", "inventory.csv"),
                           sep = ";", dec = ".", verbose = FALSE)
      target <- test_inv[c(17, 20, 22:26), ]
      test4 <- define_target(inv = test_inv, target_source = target,
                             verbose = FALSE)
    })

    # correct rows are returned
    expect_equal(test_inv$id[c(17, 20, 22:26)], test4$id[test4$target])

    # columns are returned in the correct order (target after coordinates)
    expect_equal(names(test4), c("id", "x", "y",
                                 "target", "target_id", "dbh"))

    # type is passed on
    expect_equal(attr(test4, "target_type"), "inventory")

    # it works when no dbh is available
    expect_no_error({
      define_target(inv = test_inv, target_source = target[, 1:3],
                    verbose = FALSE)
    })

    # test for warning with more than one coordinate set within tolerance
    expect_warning({
      target1 <- test_inv[c(15:20), ]
      define_target(inv = test_inv, target_source = target1,
                    verbose = FALSE)
    }, "More than one set of coordinates in the inventory")

    # test for error with duplicate trees in inventory
    expect_error({
      test_inv1 <- read_inv(rbind(test_inv, test_inv), verbose = FALSE)
      define_target(inv = test_inv1, target_source = target,
                    verbose = FALSE)
    }, "More than one set of coordinates in the inventory")

    # works if IDs are different
    expect_no_error({
      target1 <- target[1:2,]
      target1$id <- paste("target", target1$id)
      test5 <- define_target(inv = test_inv, target_source = target1,
                             verbose = FALSE)
    })

    # correct IDs are passed on
    expect_equal(test5$target_id[test5$target], c("target 18", "target 21"))

    # it works if there are duplicate target trees
    expect_error({
      target2 <- read_inv(rbind(target1, target1), verbose = FALSE)
      target2$id <- letters[1:4]
      define_target(inv = test_inv, target_source = target2,
                    verbose = FALSE)
    }, "More than one target tree has been matched.")

    # there is a warning when a coordinate is off
    expect_warning({
      target3 <- target1
      target3$x[2] <- 999
      define_target(inv = test_inv, target_source = target3,
                    verbose = FALSE)
    }, "No matching coordinates found"
    )

    # trying to use a target_inv as target_source results in a message
    expect_message({
      define_target(inv = test_inv, target_source = test5,
                    verbose = FALSE)
    }, "target_source already is of class target_inv."
    )

    # plotting with plot_target works
    expect_no_error(plot_target(test5, radius = 5))
  })


test_that(
  "methods specified as character strings  are handled correctly", {
    # target can be definedfor all trees, but results in warning
    expect_warning({
      test_inv <- read_inv(test_path("testdata", "inventory.csv"),
                           sep = ";", dec = ".", verbose = FALSE)
      test6 <- define_target(inv = test_inv, target_source = "all_trees")
    },
    "Defining all trees as target trees is rarely a good idea.")
    # type is passed on
    expect_equal(attr(test6, "target_type"), "all_trees")

    # target can be defined by removing only edge trees
    expect_no_error({
      test7 <- define_target(inv = test_inv, target_source = "exclude_edge",
                             radius = 10)
    })
    # attributes are passed on
    expect_equal(attr(test7, "target_type"), "exclude_edge")
    expect_equal(attr(test7, "spatial_radius"), 10)
    # plotting with plot_target works and radius is passed on here
    expect_no_error(plot_target(test7))

    # target can be defined by removing edge trees with a buffer around
    # the plot margin
    expect_no_error({
      test8 <- define_target(inv = test_inv,
                             target_source = "buff_edge",
                             radius = 5)
    })
    # type is passed on
    expect_equal(attr(test8, "target_type"), "buff_edge")
    expect_equal(attr(test8, "spatial_radius"), 5)

    # plotting with plot_target works and radius is passed on here
    expect_no_error(plot_target(test8))

    # print methods works
    expect_output(print(test8), "'target_inv' class inventory with target")

  })


test_that(
  "extra columns are handled correctly", {
    # target can be created with dataset with extra column
    expect_no_error({
      test_inv <- read_inv(test_path("testdata", "inventory.csv"),
                           sep = ";", dec = ".", verbose = FALSE)
      test_inv$extra <- 1
      test <- define_target(inv = test_inv)
    })

    # columns are ordered correctly (target after coord, extra at the end)
    expect_equal(names(test), c("id", "x", "y",
                                "target", "dbh", "extra"))

   })
