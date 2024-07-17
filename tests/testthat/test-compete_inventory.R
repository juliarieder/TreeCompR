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

  # check if the correct indices are returned
  expect_equal(test1$CI_Hegyi, test8$CI_Hegyi)
  expect_equal(test2$CI_RK1, test8$CI_RK1)
  expect_equal(test3$CI_RK2, test8$CI_RK2)
  expect_equal(test5$CI_Braathe, test8$CI_Braathe)
  expect_equal(test6$CI_RK3, test8$CI_RK3)
  expect_equal(test7$CI_RK4, test8$CI_RK4)

  # test plot output
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

