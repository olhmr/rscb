context("Caching: scb_create_cache and cache integrity")

testthat::test_that("a small subset of the database can be cached and accessed", {

  scb_cache_test <- scb_create_cache(initial_id = "AM/AM0101")
  testthat::expect_true(is.data.frame(scb_cache_test))
  testthat::expect_true(nrow(scb_cache_test[grepl(pattern = "AM/AM0101", x = scb_cache_test$id), ]) > 0)

})

testthat::test_that("the cache has all expected columns", {

  scb_cache_test <- scb_create_cache(initial_id = "AM/AM0101")
  testthat::expect_true("id" %in% names(scb_cache_test))
  testthat::expect_true("type" %in% names(scb_cache_test))
  testthat::expect_true("depth" %in% names(scb_cache_test))
  testthat::expect_true("name" %in% names(scb_cache_test))
  testthat::expect_true("updated" %in% names(scb_cache_test))
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

  new_cache <- scb_create_cache(initial_id = "AM/AM0101")
  new_cache <- new_cache[, !names(new_cache) == "updated"]
  # Comparing updated fails with POSIXct not provided an origin, and also better
  # to not include as difference doesn't invalidate test

  testthat::expect_equal_to_reference(new_cache,
                                      file = load("scb_cache_test.rda"))

})
