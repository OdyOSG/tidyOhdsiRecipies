test_that("test CirceR to CDM CohortSet", {
  cdm <- tidyOhdsiRecipies::tidyCdmMock()
  caprCs1 <- Capr::cs(4133224, name = "lobar pneumonia")
  caprCs2 <- Capr::cs(45176377, name = "velcade")

  csWithDetails <- purrr::map(
    list(caprCs1, caprCs2), ~
      tidyOhdsiRecipies::getCaprCsDetails(.x, cdm)
  )
  cohortsBasedOnCs <- purrr::map(
    csWithDetails, ~ tidyOhdsiRecipies::createCaprConceptSetCohort(.x, addSourceCriteria = TRUE)
  )

  named_cohort_list <- list(
    Lobar_pneumonia = cohortsBasedOnCs[[1]],
    velcade = cohortsBasedOnCs[[2]]
  )

  cohortSet <- CirceR2CDMConn(named_cohort_list)

  testthat::expect_s3_class(cohortSet, "CohortSet")
})

test_that("test Coh 2 Create to CDM CohortSet", {
  path <- fs::path(fs::path_package("tidyOhdsiRecipies"), "cohorts")
  cohortsToCreate <- tidyOhdsiRecipies::createCohortsToCreate(path)
  CohortSet <- cohortsToCreate2CDMConn(cohortsToCreate)
  testthat::expect_s3_class(CohortSet, "CohortSet")
})
