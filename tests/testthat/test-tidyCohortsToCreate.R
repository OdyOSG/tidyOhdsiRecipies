test_that("write cohort test", {
  cohort <- jsonlite::read_json(fs::path(
    fs::path_package("tidyOhdsiRecipies"), "cohorts", "PHN.json"
  ))
  path <- tempfile("tmp.json")
  on.exit(fs::file_delete(path))
  tidyOhdsiRecipies::writeListCohort(cohort, path)
  .res <- jsonlite::read_json(path)
  testthat::expect_type(.res, "list")
})

test_that("test CohortsToCreate", {
  path <- fs::path(fs::path_package("tidyOhdsiRecipies"), "cohorts")
  .res <- tidyOhdsiRecipies::createCohortsToCreate(path)
  testthat::expect_named(
    .res, c(
      "cohortId", "cohortName", "json",
      "sql"
    )
  )
  cohort <- jsonlite::read_json(fs::path(
    fs::path_package("tidyOhdsiRecipies"), "cohorts", "PHN.json"
  ))
  .res <- tidyOhdsiRecipies::createCohortsToCreate(
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
