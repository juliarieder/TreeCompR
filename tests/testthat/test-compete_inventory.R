# testthat-based unit tests for the functionality of the functions for
# inventory-based competition indices

test_that("Indices work for two different forest_inv datasets", {
  # read plot dataset
  plot <- read_inv(test_path("testdata", "inventory.csv"), verbose = FALSE)
  # read target tree dataset
  target <- read_inv(test_path("testdata", "targettrees_inventory.csv"),
                     id = "ID_target", verbose = FALSE)

  # compute Hegyi
  expect_no_error({
    test1 <- compete_inv(
      inv_source = plot,
      target_source = target,
      radius = 10,
      method = "CI_Hegyi",
      dbh_unit = "m",
      height_unit = "m",
      verbose = FALSE
    )
  })

  # compute RK1
  expect_no_error({
    test2 <- compete_inv(
      inv_source = plot,
      target_source = target,
      radius = 10,
      method = "CI_RK1",
      dbh_unit = "m",
      height_unit = "m",
      verbose = FALSE)
  })

  # compute RK2
  expect_no_error({
    test3 <- compete_inv(
      inv_source = plot,
      target_source = target,
      radius = 10,
      method = "CI_RK2",
      dbh_unit = "m",
      height_unit = "m",
      verbose = FALSE)
  })

  # compute all dbh-based indices
  expect_no_error({
    test4 <- compete_inv(
      inv_source = plot,
      target_source = target,
      radius = 10,
      method = "all_methods",
      dbh_unit = "m",
      height_unit = "m",
      verbose = FALSE)
  })

  # compute Braathe
  expect_no_error({
    # get fake heights
    plot$height <- plot$dbh / 10
    test5 <- compete_inv(
      inv_source = plot,
      target_source = target,
      radius = 10,
      method = "CI_Braathe",
      dbh_unit = "m",
      height_unit = "m",
      verbose = FALSE)
  })

  # compute RK3
  expect_no_error({
    test6 <- compete_inv(
      inv_source = plot,
      target_source = target,
      radius = 10,
      method = "CI_RK3",
      dbh_unit = "m",
      height_unit = "m",
      verbose = FALSE)
  })

  # compute RK4
  expect_no_error({
    test7 <- compete_inv(
      inv_source = plot,
      target_source = target,
      radius = 10,
      method = "CI_RK4",
      dbh_unit = "m",
      height_unit = "m",
      verbose = FALSE)
  })

  # compute all indices
  expect_no_error({
    test8 <- compete_inv(
      inv_source = plot,
      target_source = target,
      radius = 10,
      method = "all_methods",
      dbh_unit = "m",
      height_unit = "m",
      verbose = FALSE)
  })

  # compute subset of indices indices
  expect_no_error({
    test9 <- compete_inv(
      inv_source = plot,
      target_source = target,
      radius = 10,
      method = c("CI_Hegyi", "CI_RK3", "CI_RK4"),
      dbh_unit = "m",
      height_unit = "m",
      verbose = FALSE)
  })

  # correct indices are returned
  expect_equal(test1$CI_Hegyi, test8$CI_Hegyi)
  expect_equal(test2$CI_RK1, test8$CI_RK1)
  expect_equal(test3$CI_RK2, test8$CI_RK2)
  expect_equal(test5$CI_Braathe, test8$CI_Braathe)
  expect_equal(test6$CI_RK3, test8$CI_RK3)
  expect_equal(test7$CI_RK4, test8$CI_RK4)
  expect_equal(grep("CI_", names(test9), value = TRUE),
               c("CI_Hegyi", "CI_RK3", "CI_RK4"))

  # class is maintained after rbind
  expect_true(
    inherits(rbind(test1, test1), "compete_inv")
  )

  # plot output works
  expect_no_error(plot_target(test8))
})


test_that("Indices work for two file paths", {
  expect_warning(
    compete_inv(
      inv_source = test_path("testdata", "inventory.csv"),
      target_source = test_path("testdata", "inventory.csv"),
      radius = 10,
      method = "all_methods",
      dbh_unit = "m",
      tol = 0.01, # set to low value to avoid warning b/c close coordinates
      verbose = FALSE),
    regexp = "Defining all trees as target trees"
  )
})

test_that("Indices work for other types of target determination", {
  # read plot dataset
  plot <- read_inv(test_path("testdata", "inventory.csv"),
                   verbose = FALSE)
  target <- plot[1:5,]
  # select by id
  expect_no_error(
    test1 <- compete_inv(
      inv_source = plot,
      target_source = target$id,
      radius = 10,
      method = "all_methods",
      dbh_unit = "m",
      verbose = FALSE)
  )

  # compare against selection by logical vector
  expect_equal(
    test1$CI_Hegyi,
    compete_inv(
      inv_source = plot,
      target_source = plot$id %in% target$id,
      radius = 10,
      method = "all_methods",
      dbh_unit = "m",
      verbose = FALSE)$CI_Hegyi
  )

  # compare against selection by subset
  # select by id
  expect_equal(
    test1$CI_RK1,
    compete_inv(
      inv_source = plot,
      target_source = target,
      radius = 10,
      method = "all_methods",
      dbh_unit = "m",
      verbose = FALSE)$CI_RK1
  )

  # try if it works with methods specified by character-strings
  expect_no_error(compete_inv(
    inv_source = plot,
    target_source = "exclude_edge",
    radius = 10,
    method = "all_methods",
    dbh_unit = "m",
    verbose = FALSE)
  )
  expect_no_error(compete_inv(
    inv_source = plot,
    target_source = "buff_edge",
    radius = 10,
    method = "all_methods",
    dbh_unit = "m",
    verbose = FALSE)
  )
  expect_warning(compete_inv(
    inv_source = plot,
    target_source = "all_trees",
    radius = 10,
    method = "all_methods",
    dbh_unit = "m",
    verbose = FALSE)
  )
})

test_that("Function works on target_inv objects", {
  expect_no_error({
    plot <- read_inv(test_path("testdata", "inventory.csv"), verbose = FALSE)
    targets <- define_target(plot,target_source = "buff_edge", radius = 10)
    CI <- compete_inv(inv_source = targets, radius = 10,
                      method = "all_methods")
  })
})


test_that("Printing works", {
  expect_output(
    print(
      compete_inv(
        inv_source = test_path("testdata", "inventory.csv"), radius = 10,
        method = "CI_Hegyi", verbose = FALSE)
    ),
    "'compete_inv' class inventory with distance-based competition indices"
  )
})

test_that("Handling of kmax works", {
  # warning a kmax that is too low
  expect_warning({
    plot <- read_inv(test_path("testdata", "inventory.csv"), verbose = FALSE)
    targets <- define_target(plot,target_source = "buff_edge", radius = 5)
    CI <- compete_inv(inv_source = targets, radius = 5, kmax = 5,
                      method = "all_methods")
  },
  "Maximum number of target trees reached"
  )
})


test_that("Function works with manual column specifications", {
  # specification with quoted names
  expect_no_error(
    test1 <- compete_inv(
      inv_source = test_path("testdata", "inventory1.csv"),
      target_source = "buff_edge",
      id = "TreeID",
      x = "X",
      y = "Y",
      dbh = "DiaBrHgt",
      radius = 10,
      method = "all_methods",
      dbh_unit = "m",
      verbose = FALSE)
  )
  # specification with unquoted names
  expect_equal(
    test1,
    compete_inv(
      inv_source = test_path("testdata", "inventory1.csv"),
      target_source = "buff_edge",
      id = TreeID,
      x = X,
      y = Y,
      dbh = DiaBrHgt,
      radius = 10,
      method = "all_methods",
      dbh_unit = "m",
      verbose = FALSE)
  )
  # specification with mixed  names
  expect_equal(
    test1,
    compete_inv(
      inv_source = test_path("testdata", "inventory1.csv"),
      target_source = "buff_edge",
      id = TreeID,
      x = "X",
      y = Y,
      dbh = "DiaBrHgt",
      radius = 10,
      method = "all_methods",
      dbh_unit = "m",
      verbose = FALSE)
  )
  # specification with mixed and partially unspecified names
  expect_equal(
    test1,
    compete_inv(
      inv_source = test_path("testdata", "inventory1.csv"),
      target_source = "buff_edge",
      id = TreeID,
      dbh = "DiaBrHgt",
      radius = 10,
      method = "all_methods",
      dbh_unit = "m",
      verbose = FALSE)
  )
})

test_that("Function works for nonstandard indices", {
  plot <- read.csv(test_path("testdata", "inventory.csv"), sep = ";")
  plot$test <- plot$DBH * abs(plot$X - plot$Y)

  # dataset with size can be loaded
  expect_contains({
    # read plot dataset
    plot <- read_inv(plot, size = test, verbose = FALSE)
    names(plot)},
    "size"
  )

  # "all_methods" contains "size"
  expect_contains({
    # read plot dataset
    comp <- compete_inv(plot, verbose = FALSE, radius = 5)
    names(comp)},
    "CI_size"
  )
  # not all values are zero
  expect_true(sum(comp[, "CI_size"]) > 0)

  # a function can be defined
  expect_no_error({
    assign("CI_inv_Hegyi",
           function(target, neigh) sum(target$dbh * neigh$dist / neigh$dbh),
           envir = .GlobalEnv)
    out <-  compete_inv(plot, verbose = FALSE, radius = 5,
                        method = c("CI_Hegyi", "CI_inv_Hegyi"))
  })

  # correct name is shown in table
  expect_contains(names(out), "CI_inv_Hegyi")

  # custom functions can be mixed with all methods
  expect_no_error({
    out <-  compete_inv(plot, verbose = FALSE, radius = 5,
                        method = c("all_methods", "CI_inv_Hegyi"))
  })

  # correct names are shown in table
  expect_contains(names(out), c("CI_Hegyi", "CI_RK1", "CI_RK2",
                                "CI_inv_Hegyi", "CI_size"))

  # custom functions can be used for extra columns
  expect_no_error({
    test <- read.csv(
      test_path("testdata", "inventory.csv"),
      sep = ";", dec = "."
    )
    test$extra <- grepl("1", test$TreeID)

    # assign function
    assign("CI_test",
           function(target, neigh) {
             sum(target$dbh * (neigh$dist / neigh$dbh)[neigh$extra])
           },
           envir = .GlobalEnv)

    # create output
    out1 <-  compete_inv(test, keep_rest = TRUE, verbose = FALSE,
                         radius = 5, method = c("CI_test"))
  })

  # output makes sense
  expect_gt(sum(out1$CI_test), 0)

  # a function with the wrong arguments fails
  expect_error({
    compete_inv(plot, verbose = FALSE, radius = 5,
                method = c("CI_Hegyi", "plot"))
  }, "Invalid competition index function")

  # not a function fails
  expect_error({
    compete_inv(plot, verbose = FALSE, radius = 5,
                method = c("CI_Hegyi", "CI_Heygi"))
  },  "Invalid competition index function")

  # duplicate indices are removed
  expect_length({
    compete_inv(plot, verbose = FALSE, radius = 5,
                method = c("CI_Hegyi", "CI_Hegyi"))
  }, 6)

})

