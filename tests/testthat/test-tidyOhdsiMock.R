library(testthat)
test_that("Testing CDM Mock for testeing ", {
  cdm <- tidyOhdsiRecipies::tidyCdmMock()
  expect_s3_class(cdm, "cdm_reference")
  CDMConnector::cdmDisconnect(cdm)
})


test_that("Testing SqLite Connection for testing", {
  con <- tidyOhdsiRecipies::returnSqLiteDatabaseConnectorCon()
  testthat::expect_s4_class(con, "DatabaseConnectorDbiConnection")
  .res <- DatabaseConnector::querySql(con, 'select * from main.concept')
  testthat::expect_s3_class(.res, 'data.frame')
  DatabaseConnector::disconnect(con)
})
