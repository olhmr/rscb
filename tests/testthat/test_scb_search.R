context("Search: scb_search")

load("scb_cache_test.rda")

testthat::test_that("ambiguous arguments give warning", {
  
  testthat::expect_warning(scb_search(cached_database = scb_cache_test, 
                                      search_type = "l", search_var_desc = "not null"))
  testthat::expect_warning(scb_search(cached_database = scb_cache_test, 
                                      search_val_desc = "something"))
  
})

testthat::test_that("empty data.frame is returned when there is no match", {
  
  testthat::expect_equal(nrow(scb_search(cached_database = scb_cache_test, 
                                         search_type = "not a valid type")), 0)
  testthat::expect_equal(nrow(scb_search(cached_database = scb_cache_test, 
                                         search_id = "apricots")), 0)
  testthat::expect_equal(nrow(scb_search(cached_database = scb_cache_test, 
                                         search_type = "t", search_var_desc = "pears per capita squared")), 0)
  
})

testthat::test_that("data.frame is returned", {
  
  testthat::expect_true(is.data.frame(scb_search(cached_database = scb_cache_test)))
  testthat::expect_true(is.data.frame(scb_search(cached_database = scb_cache_test,
                                                 search_type = "l")))
  testthat::expect_true(is.data.frame(scb_search(cached_database = scb_cache_test,
                                                 search_type = "t", search_var_desc = "observations")))
  
})

testthat::test_that("returned data.frame matches filters", {
  
  testthat::expect_true(sum(grepl(pattern = "observations", 
                                  x = scb_search(cached_database = scb_cache_test, 
                                                 search_type = "t", 
                                                 search_var_desc = "observations")$var_desc)) > 0)
  
})