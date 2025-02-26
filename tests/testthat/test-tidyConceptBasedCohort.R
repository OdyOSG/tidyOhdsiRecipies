library(testthat)
test_that("Test Cohorts Programmatic definition", {
  cohortDonor <- jsonlite::read_json(fs::path(
    fs::path_package("tidyOhdsiRecipies"), "cohorts", "PHN.json"
  ))
  caprConceptSets <- tidyOhdsiRecipies::collectCaprCsFromCohort(cohortDonor)[1:2]
  cohs <- purrr::map(
    caprConceptSets,
    ~ tidyOhdsiRecipies::createCaprConceptSetCohort(.x)
  )
  expect_false(is.null(cohs[[1]]$ConceptSets))
  expect_false(is.null(cohs[[2]]$ConceptSets))
  expect_length(cohs, 2)
})
