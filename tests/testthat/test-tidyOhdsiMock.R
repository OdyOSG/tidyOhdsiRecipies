library(testthat)
test_that("Testing CDM Mock for testeing ", {
  cdm <- tidyOhdsiRecipies::tidyCdmMock()
  on.exit(CDMConnector::cdmDisconnect(cdm))
  expect_s3_class(cdm, "cdm_reference")
})


test_that("Testing SqLite Connection for testing", {
  con <- tidyOhdsiRecipies::returnSqLiteDatabaseConnectorCon()
  on.exit(DatabaseConnector::disconnect(con))
  testthat::expect_s4_class(con, "DatabaseConnectorDbiConnection")
  .res <- DatabaseConnector::querySql(con, 'select * from main.concept')
  testthat::expect_s3_class(.res, 'data.frame')
})
