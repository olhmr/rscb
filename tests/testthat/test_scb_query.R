context("Quering: scb_query")

testthat::test_that("using two filters give the intersection of each filter", {

  # This relies on the elimination procedure explained in the API documentation
  # at www.scb.se/api.

  filter_1 <- scb_query(table_id = "AM/AM0101/AM0101A/LonArb07Privat",
                        list(code = "SNI2007", filter = "item", values = c("B", "C", "D")))$parsed_data
  intersection <- as.data.frame(filter_1[filter_1$`overtime pay` == "including overtime pay", ])
  combined <- as.data.frame(scb_query(table_id = "AM/AM0101/AM0101A/LonArb07Privat",
                                      list(code = "Overtidstillagg", filter = "item", values = "20"),
                                      list(code = "SNI2007", filter = "item", values = c("B", "C", "D")))$parsed_data)
  testthat::expect_equivalent(intersection, combined)

})

testthat::test_that("querying a non-existing table returns 404", {

  testthat::expect_equal(scb_query(table_id = "fake_id")$status_code, 404)

})
