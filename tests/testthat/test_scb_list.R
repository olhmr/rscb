context("List: scb_list")

testthat::test_that("scb_list with unexpected id returns 404", {

  testthat::expect_equal(scb_list(id = "bar")[[1]], 404)
  testthat::expect_equal(scb_list(id = 2)[[1]], 404)

})

testthat::test_that("scb_list with unexpected database_id returns 404", {

  testthat::expect_equal(scb_list(database_id = "sssd")[[1]], 404)
  testthat::expect_equal(scb_list(database_id = 1)[[1]], 404)

})

testthat::test_that("data.frame is returned", {

  testthat::expect_true(is.data.frame(scb_list()[[2]]))

})
