test_that("Cs Transition Test", {
  caprCs <- tidyOhdsiRecipies::darwinCSExpression2CaprCs(
    list(test_set = dplyr::tibble(
      concept_id = c(1, 2),
      excluded = c(FALSE, TRUE),
      descendants = c(TRUE, FALSE),
      mapped = c(FALSE, TRUE)
    ))
  )
  testthat::expect_s4_class(caprCs, "ConceptSet")
})


testthat::test_that("Cs To List Concept Ids", {
  library(DatabaseConnector)
  con <- tidyOhdsiRecipies::returnSqLiteDatabaseConnectorCon()
  diclofenac <- Capr::cs(Capr::descendants(1124300), name = "diclofenac")
  dummyCs <- Capr::cs(10, name = "dummy")
  diclofenacConceptIds <- tidyOhdsiRecipies::listConceptIdsFromCs(diclofenac, con, vocabularyDatabaseSchema = "main")
  testthat::expect_contains(diclofenacConceptIds, 40032408)
  testthat::expect_error(tidyOhdsiRecipies::listConceptIdsFromCs(dummyCs, con, vocabularyDatabaseSchema = "main"))
  disconnect(con)
})


test_that("mergeCsAndConcepts", {
  cohortDonor <- jsonlite::read_json(fs::path(
    fs::path_package("tidyOhdsiRecipies"), "cohorts", "PHN.json"
  ))
  caprConceptSets <- tidyOhdsiRecipies::collectCaprCsFromCohort(cohortDonor)[1]
  .res <- mergeCsAndConcepts(caprConceptSets, conceptIds = 2:4)
  testthat::expect_s4_class(.res, "ConceptSet")
})
