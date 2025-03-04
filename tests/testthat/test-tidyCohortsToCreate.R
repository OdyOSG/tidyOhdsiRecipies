test_that("write cohort test", {
  cohort <- jsonlite::read_json(fs::path(
    fs::path_package("tidyOhdsiRecipies"), "cohorts", "PHN.json"
  ))[[1]]

  tidyOhdsiRecipies::writeListCohort(cohort, cohortName = "testing", saveLocation = ".")
  .res <- jsonlite::read_json("testing.json")
  testthat::expect_type(.res, "list")
  fs::file_delete("testing.json")
})

test_that("test CohortsToCreate", {
  path <- fs::path(fs::path_package("tidyOhdsiRecipies"), "cohorts")
  .res <- tidyOhdsiRecipies::createCohortDefinitionSet(path)
  testthat::expect_named(
    .res, c(
      "cohortId", "cohortName", "json",
      "sql"
    )
  )
  cohort <- jsonlite::read_json(fs::path(
    fs::path_package("tidyOhdsiRecipies"), "cohorts", "PHN.json"
  ))
  .res <- tidyOhdsiRecipies::createCohortDefinitionSet(
    cohorts = list(
      "test" = cohort
    )
  )
  testthat::expect_named(
    .res, c(
      "cohortId", "cohortName", "json",
      "sql"
    )
  )
})
