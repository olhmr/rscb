context("Caching: scb_create_cache")

testthat::test_that("a small subset of the database can be cached and searched", {

  testthat::expect_true(is.data.frame(scb_create_cache(initial_id = "AM/AM0101")))
  scb_cache_test <- scb_create_cache(initial_id = "AM/AM0101")
  testthat::expect_equal(scb_cache_test, scb_search(cached_database = scb_cache_test))
  testthat::expect_equal(scb_cache_test[1, ]$id, "AM0101A")

})

testthat::test_that("unexpected arguments give intelligible errors", {

  testthat::expect_warning(scb_create_cache(lang = "kr"))
  testthat::expect_warning(scb_create_cache(initial_id = 3))

})
