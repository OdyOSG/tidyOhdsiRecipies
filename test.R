
es <- AlexSettings::returnExecutionSettings()
con <-
  es$connectionDetails |>
  DatabaseConnector::connect()
