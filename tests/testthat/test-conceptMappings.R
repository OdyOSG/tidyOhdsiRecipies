test_that("Mapping and Gathering", {
  cdm <- tidyOhdsiRecipies::tidyCdmMock()
  conceptIds <- tidyOhdsiRecipies::conceptIdsFromSources(cdm, c("63020004901"), c("NDC"))
  testthat::expect_equal(conceptIds, 45176377)
  standardConcepts <- tidyOhdsiRecipies::standardFromSourceConceptIds(cdm, conceptIds)
  testthat::expect_equal(standardConcepts, 19102219)
  icd10_with_dot <- tidyOhdsiRecipies::addDot("A001")
  testthat::expect_equal(icd10_with_dot, "A00.1")
})
