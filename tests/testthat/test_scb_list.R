context("List: scb_list")

testthat::test_that("scb_list with unexpected id returns 404", {
  
  testthat::expect_equal(scb_list(id = "bar"), "Unexpected status code from GET: 404")
  testthat::expect_equal(scb_list(id = 2), "Unexpected status code from GET: 404")
  
})

testthat::test_that("scb_list with unexpected language stops", {
  
  testthat::expect_error(scb_list(lang = "fr"))
  testthat::expect_error(scb_list(lang = 4))
  
})

testthat::test_that("scb_list with unexpected database_id returns 404", {
  
  testthat::expect_equal(scb_list(database_id = "sssd"), "Unexpected status code from GET: 404")
  testthat::expect_equal(scb_list(database_id = 1), "Unexpected status code from GET: 404")
  
})

testthat::test_that("data.frame is returned", {
  
  testthat::expect_true(is.data.frame(scb_list()))
  
})