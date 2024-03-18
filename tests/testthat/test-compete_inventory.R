# testthat-based unit tests for the functionality of the functions for
# inventory-based competition indices

test_that("Hegyi index works for inv tables with dbh values", {
  expect_no_error(
    test1 <- compete_inv(inv_source = test_path("testdata", "inventory.csv"),
                 target_source = test_path("testdata", "targettrees_inventory.csv"),
                 radius = 10, method = "CI_Hegyi", dbh_unit = "m",
                 height_unit = "m")
    )
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

