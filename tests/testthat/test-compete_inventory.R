# testthat-based unit tests for the functionality of the functions for
# inventory-based competition indices

test_that("Indices work for two different forest_inv datasets", {

  expect_no_error({
    # read plot dataset
    plot <- read_inv(test_path("testdata", "inventory.csv"), verbose = FALSE)
    # read target tree dataset
    target <- read_inv(test_path("testdata", "targettrees_inventory.csv"),
                       id = "ID_target")
    # compute Hegyi
    test1 <- compete_inv(
      inv_source = plot,
      target_source = target,
      radius = 10,
      method = "CI_Hegyi",
      dbh_unit = "m",
      height_unit = "m")
  })
  # compute RK1
  expect_no_error({
    test2 <- compete_inv(
      inv_source = plot,
      target_source = target,
      radius = 10,
      method = "CI_RK1",
      dbh_unit = "m",
      height_unit = "m")
  })

  # compute RK2
  expect_no_error({
    test3 <- compete_inv(
      inv_source = plot,
      target_source = target,
      radius = 10,
      method = "CI_RK2",
      dbh_unit = "m",
      height_unit = "m")
  })

  # compute all dbh-based indices
  expect_no_error({
    test4 <- compete_inv(
      inv_source = plot,
      target_source = target,
      radius = 10,
      method = "all",
      dbh_unit = "m",
      height_unit = "m")
  })

  # test plot output
  expect_no_error(plot_target(test4))
})
