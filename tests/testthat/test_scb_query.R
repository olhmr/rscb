context("Quering: scb_query")

testthat::test_that("unexpected arguments give intelligible errors", {

  testthat::expect_warning(scb_query(table_id = "AM/AM0101/AM0101A/LonArb07Privat",
                                     list(code = "Overtidstillagg", filter = "top", values = "2"),
                                     lang = "kr"))
  testthat::expect_warning(scb_query(table_id = "AM/AM0101/AM0101A/LonArb07Privat",
                                     list(code = "Overtidstillagg", filter = "top", values = "2"),
                                     database_id = "ssb"))
  testthat::expect_warning(scb_query(table_id = "AM/AM0101/AM0101A/LonArb07Privat"))

})

testthat::test_that("using two filters give the intersection of each filter", {

  # This relies on the elimination procedure explained in the API documentation
  # at www.scb.se/api.

  filter_1 <- scb_query(table_id = "AM/AM0101/AM0101A/LonArb07Privat",
                        list(code = "SNI2007", filter = "item", values = c("B", "C", "D")))
  intersection <- as.data.frame(filter_1[filter_1$`overtime pay` == "including overtime pay", ])
  combined <- as.data.frame(scb_query(table_id = "AM/AM0101/AM0101A/LonArb07Privat",
                                      list(code = "Overtidstillagg", filter = "item", values = "20"),
                                      list(code = "SNI2007", filter = "item", values = c("B", "C", "D"))))
  testthat::expect_equivalent(intersection, combined)

})

testthat::test_that("querying a non-existing table returns 404", {

  testthat::expect_equal(scb_query(table_id = "fake_id"), "Unexpected status code from POST: 404")

})
