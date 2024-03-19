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
      height_unit = "m")
  })

  # compute RK3
  expect_no_error({
    test6 <- compete_inv(
      inv_source = plot,
      target_source = target,
      radius = 10,
      method = "CI_RK3",
      dbh_unit = "m",
      height_unit = "m")
  })

  # compute RK4
  expect_no_error({
    test7 <- compete_inv(
      inv_source = plot,
      target_source = target,
      radius = 10,
      method = "CI_RK4",
      dbh_unit = "m",
      height_unit = "m")
  })

  # compute all indices
  expect_no_error({
    test8 <- compete_inv(
      inv_source = plot,
      target_source = target,
      radius = 10,
      method = "all",
      dbh_unit = "m",
      height_unit = "m")
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


  expect_no_error(
    compete_inv(
      inv_source = test_path("testdata", "inventory.csv"),
      target_source = test_path("testdata", "inventory.csv"),
      radius = 10,
      method = "all",
      dbh_unit = "m",
      verbose = FALSE)
  )

})


# remove plot output
suppressWarnings(x <- file.remove(test_path("Rplots.pdf")))

