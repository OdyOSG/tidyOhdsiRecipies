
es <- AlexSettings::returnExecutionSettings()
con <-
  es$connectionDetails |>
  DatabaseConnector::connect()


cohortDonor <- jsonlite::read_json(fs::path(
  fs::path_package("tidyOhdsiRecipies"), "cohorts", "PHN.json"
))
caprConceptSets <- tidyOhdsiRecipies::collectCaprCsFromCohort(cohortDonor)[1]
