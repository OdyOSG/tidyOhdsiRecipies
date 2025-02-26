library(tidyOhdsiRecipies)
library(testthat)
test_that("Concepts From Cohort", {
  cohortDonor <- jsonlite::read_json(fs::path(
    fs::path_package("tidyOhdsiRecipies"), "cohorts", "PHN.json"
  ))
  caprConceptSets <- collectCaprCsFromCohort(cohortDonor)
  testthat::expect_s4_class(caprConceptSets[[1]], "ConceptSet")
  testthat::expect_vector(names(caprConceptSets), "character")
})
test_that("Cs Details ", {
  cdm <- tidyOhdsiRecipies::tidyCdmMock()
  caprCs1 <- 4133224L
  csWithDetails <- tidyOhdsiRecipies::getCaprCsDetails(caprCs1, cdm)
  expect_equal(
    csWithDetails@Expression[[1]]@Concept@concept_name,
    "Lobar pneumonia"
  )
  CDMConnector::cdmDisconnect(cdm)
})
test_that("Cs Candidates", {
  con <- tidyOhdsiRecipies::returnSqLiteDatabaseConnectorCon()
  caprCand <- tidyOhdsiRecipies::collectCandidatesToCapr(
  con, 'main', c('pneumonia'))
  DatabaseConnector::disconnect(con)
  testthat::expect_s4_class(caprCand, 'ConceptSet')
})
