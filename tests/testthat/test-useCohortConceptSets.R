library(tidyOhdsiRecipies)
library(testthat)
test_that("multiplication works ", {
  cohortDonor <- jsonlite::read_json(fs::path(
  fs::path_package("tidyOhdsiRecipies"), "cohorts", "PHN.json"
  ))
  caprConceptSets <- collectCaprCsFromCohort(cohortDonor)
  testthat::expect_s4_class(caprConceptSets[[1]], 'ConceptSet')
  testthat::expect_vector(names(caprConceptSets), 'character')
})
