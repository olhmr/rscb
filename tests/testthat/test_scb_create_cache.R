context("Caching: scb_create_cache and cache integrity")

testthat::test_that("a small subset of the database can be cached and accessed", {

  testthat::expect_true(is.data.frame(scb_create_cache(initial_id = "AM/AM0101")))
  scb_cache_test <- scb_create_cache(initial_id = "AM/AM0101")
  testthat::expect_equal(scb_cache_test, scb_search(cached_database = scb_cache_test))
  testthat::expect_equal(scb_cache_test[1, ]$id, "AM0101A")

})

testthat::test_that("the cache has all expected columns", {

  scb_cache_test <- scb_create_cache(initial_id = "AM/AM0101")
  testthat::expect_true("id" %in% names(scb_cache_test))
  testthat::expect_true("depth" %in% names(scb_cache_test))
  testthat::expect_true("type" %in% names(scb_cache_test))
  testthat::expect_true("name" %in% names(scb_cache_test))
  testthat::expect_true("var_desc" %in% names(scb_cache_test))
  testthat::expect_true("val_desc" %in% names(scb_cache_test))
  testthat::expect_true("date_start" %in% names(scb_cache_test))
  testthat::expect_true("date_end" %in% names(scb_cache_test))

})

testthat::test_that("tables, and only tables, have metadata", {

  scb_cache_test <- scb_create_cache(initial_id = "AM/AM0101")
  testthat::expect_equal(nrow(scb_cache_test[scb_cache_test$type == "l" &
                                             (!is.na(scb_cache_test$var_desc)) &
                                             (!is.na(scb_cache_test$val_desc)) &
                                             (!is.na(scb_cache_test$date_start)) &
                                             (!is.na(scb_cache_test$date_end)), ]), 0)
  testthat::expect_equal(nrow(scb_cache_test[scb_cache_test$type == "t" &
                                               (is.na(scb_cache_test$var_desc)) &
                                               (is.na(scb_cache_test$val_desc)) &
                                               (is.na(scb_cache_test$date_start)) &
                                               (is.na(scb_cache_test$date_end)), ]), 0)

})

testthat::test_that("the same cache is created from the same input", {

  testthat::expect_equal_to_reference(scb_create_cache(initial_id = "AM/AM0101"),
                                      file = load("scb_cache_test.rda"))

})
