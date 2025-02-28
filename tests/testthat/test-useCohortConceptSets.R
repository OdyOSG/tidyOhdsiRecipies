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
  caprCand <- suppressWarnings(tidyOhdsiRecipies::collectCandidatesToCapr(
  con, 'main', c('pneumonia'), 'lll'))
  DatabaseConnector::disconnect(con)
  testthat::expect_s4_class(caprCand, 'ConceptSet')
})



test_that("Inject Itest", {
  cohortDonor <- jsonlite::read_json(fs::path(
    fs::path_package("tidyOhdsiRecipies"), "cohorts", "PHN.json"
  ))
  cn <- "Post-herpetic polyneuropathy"
  caprConceptSets <- collectCaprCsFromCohort(cohortDonor)[1]
  modCoh <- injectItemsIntoCohort(cohortDonor, caprConceptSets$phn, position = 3,
                                    writeCohortPath = NULL)
  testthat::expect_equal(
    cn,
    modCoh$ConceptSets[[3]]$expression$items[[1]]$concept$CONCEPT_NAME)
  tempF <- tempfile('t.json')
  modCoh <- injectItemsIntoCohort(cohortDonor, caprConceptSets$phn, position = 3,
                                  writeCohortPath = tempF)
  .res <- jsonlite::read_json(tempF)

  testthat::expect_type(.res, "list")

  fs::file_delete(tempF)

})
