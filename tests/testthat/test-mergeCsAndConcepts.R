test_that("mergeCsAndConcepts", {
  cohortDonor <- jsonlite::read_json(fs::path(
    fs::path_package("tidyOhdsiRecipies"), "cohorts", "PHN.json"
  ))
  caprConceptSets <- tidyOhdsiRecipies::collectCaprCsFromCohort(cohortDonor)[1]
  .res <- mergeCsAndConcepts(caprConceptSets, conceptIds = 2:4)
  testthat::expect_s4_class(.res, 'ConceptSet')
})
