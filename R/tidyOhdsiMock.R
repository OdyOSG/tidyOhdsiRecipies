#' Create Mock CDM Reference
#'
#' This function creates mock CDM data reference for testing purposes.
#'
#' @return cdm_reference via CDMConnector.
#' @export
#'
#' @examples
#' \dontrun{
#' mock_cdm <- tidyCdmMock()
#' }
tidyCdmMock <- function() {
  numberIndividuals <- 10
  rlang::check_installed("duckdb")
  con <- duckdb::dbConnect(duckdb::duckdb(), ":memory:")
  writeSchema <- "main"

  tables <- list(
    "concept" = data.frame(),
    "concept_relationship" = data.frame(),
    "observation_period" = data.frame(),
    "person" = data.frame(),
    "condition_occurrence" = data.frame(),
    "drug_exposure" = data.frame()
  )
  omopgenerics::assertList(tables, named = TRUE, class = "data.frame")
  tables[["concept_relationship"]] <- dplyr::tribble(
    ~concept_id_1, ~concept_id_2, ~relationship_id, ~valid_start_date, ~valid_end_date,
    35207953, 4133224, "Maps to", as.Date("2020-01-01"), as.Date("2020-01-01"),
    45176377, 19102219, "Maps to", as.Date("2020-01-01"), as.Date("2020-01-01")
  )
  tables[["concept"]] <- dplyr::tribble(
    ~concept_id, ~concept_name, ~concept_code, ~vocabulary_id,
    ~domain_id, ~concept_class_id,
    ~valid_start_date, ~valid_end_date, ~invalid_reason,
    ~standard_concept,
    35207953, "Lobar pneumonia, unspecified organism", "J18.1", "ICD10CM",
    "Condition", "plug", as.Date("2020-01-01"), as.Date("2030-01-01"), "V", "N",
    4133224, "Lobar pneumonia", "278516003", "SNOMED",
    "Condition", "plug", as.Date("2020-01-01"), as.Date("2030-01-01"), "V", "S",
    45176377, "bortezomib 3.5 MG Injection [Velcade]", "63020004901", "NDC",
    "Drug", "plug", as.Date("2020-01-01"), as.Date("2030-01-01"), "V", "N",
    19102219, "bortezomib 3.5 MG Injection [Velcade]", "402244", "RxNorm",
    "Drug", "plug", as.Date("2020-01-01"), as.Date("2030-01-01"), "V", "S",
  )
  persons <- seq_len(numberIndividuals)
  n <- length(persons)
  tables[["person"]] <- dplyr::tibble(
    person_id = persons,
    gender_concept_id = as.integer(sample(
      c(8532, 8507),
      n, TRUE
    )),
    year_of_birth = 1900L + sample.int(
      80,
      n, TRUE
    ),
    race_concept_id = rep.int(0L, n),
    ethnicity_concept_id = rep.int(0L, n)
  )


  dates <- dplyr::tibble(person_id = integer(), date = as.Date(character()))
  for (k in seq_along(tables)) {
    x <- tables[[k]]
    cols <- colnames(x)
    id <- c("person_id", "subject_id")
    id <- id[id %in% cols]
  }
  tables[["observation_period"]] <- dplyr::mutate(
    dplyr::select(
      tables[["person"]],
      "person_id", "year_of_birth"
    ),
    observation_period_start_date = as.Date(NA),
    observation_period_end_date = as.Date(NA)
  )

  tables[["observation_period"]] <- dplyr::select(
    dplyr::mutate(tables[["observation_period"]],
      observation_period_start_date = dplyr::if_else(is.na(.data$observation_period_start_date),
        as.Date(x = paste0(
          .data$year_of_birth + sample.int(
            34,
            n, TRUE
          ), "-", sample(1:12, n, TRUE), "-",
          sample(1:28, n, TRUE)
        ), format = "%Y-%m-%d"),
        .data$observation_period_start_date
      ), observation_period_end_date = dplyr::if_else(is.na(.data$observation_period_end_date),
        .data$observation_period_start_date + sample.int(
          10000,
          n, TRUE
        ), .data$observation_period_end_date
      ),
      period_type_concept_id = 0L, observation_period_id = dplyr::row_number(),
      observation_period_start_date = dplyr::if_else(as.integer(format(
        .data$observation_period_start_date,
        "%Y"
      )) >= .data$year_of_birth, as.Date(paste0(
        .data$year_of_birth,
        "-01-01"
      )), .data$observation_period_start_date)
    ),
    -"year_of_birth"
  )

  tables[["person"]] <- dplyr::select(
    dplyr::mutate(dplyr::left_join(tables[["person"]],
      dplyr::summarise(dplyr::group_by(
        tables[["observation_period"]],
        .data$person_id
      ), start_year = as.integer(format(
        min(.data$observation_period_start_date),
        "%Y"
      )), .groups = "drop"),
      by = "person_id"
    ), year_of_birth = dplyr::if_else(.data$year_of_birth <=
      .data$start_year, .data$year_of_birth, .data$start_year)),
    -"start_year"
  )
  nr <- sample.int(n * 2, 1)
  tables[["drug_exposure"]] <- dplyr::mutate(
    addDate(dplyr::ungroup(dplyr::slice_sample(dplyr::group_by(
      dplyr::inner_join(
        dplyr::mutate(dplyr::tibble(person_id = sample(tables$person$person_id,
          size = nr, TRUE
        )), id = dplyr::row_number()), tables[["observation_period"]],
        by = "person_id", relationship = "many-to-many"
      ),
      .data$id
    ), n = 1)), c(
      "drug_exposure_start_date",
      "drug_exposure_end_date"
    )),
    drug_exposure_id = dplyr::row_number(),
    drug_concept_id = rep.int(19102219, nr),
    drug_source_concept_id = rep.int(45176377, nr),
    drug_type_concept_id = 0L,
    visit_occurrence_id = 1:nr
  )


  nr <- sample.int(n * 2, 1)
  tables[["condition_occurrence"]] <- dplyr::mutate(
    addDate(dplyr::ungroup(dplyr::slice_sample(dplyr::group_by(
      dplyr::inner_join(
        dplyr::mutate(dplyr::tibble(person_id = sample(tables$person$person_id,
          size = nr, TRUE
        )), id = dplyr::row_number()), tables[["observation_period"]],
        by = "person_id", relationship = "many-to-many"
      ),
      .data$id
    ), n = 1)), c("condition_start_date", "condition_end_date")),
    condition_occurrence_id = seq_len(nr),
    condition_concept_id = rep.int(4133224, nr),
    condition_source_concept_id = rep.int(35207953, nr),
    condition_type_concept_id = 0L,
    visit_occurrence_id = 1:nr
  )


  tablesToInsert <- names(tables)
  src <- CDMConnector::dbSource(con = con, writeSchema = writeSchema)
  for (tab in names(tables)) {
    x <- tables[[tab]]
    for (col in c("subject_id", "person_id", "cohort_definition_id")) {
      if (col %in% colnames(x)) {
        x <- dplyr::mutate(x, `:=`(!!col, as.integer(.data[[col]])))
      }
    }
    invisible(omopgenerics::insertTable(
      cdm = src, name = tab,
      table = x, overwrite = TRUE
    ))
  }
  cdm <- tidyCdmFromCon(
    con = con,
    cdmSchema = writeSchema,
    writeSchema = writeSchema,
    cdmName = "tidyMock"
  )
  return(cdm)
}

#' Create Test SQLite Database Connection
#'
#' This function creates and returns a connection to a SQLite database using the DatabaseConnector package.
#'
#' @return A DatabaseConnector connection object.
#' @export
#'
#' @examples
#' con <- returnSqLiteDatabaseConnectorCon()
#' DBI::dbListTables(con)
#' DatabaseConnector::disconnect(con)
returnSqLiteDatabaseConnectorCon <- function() {
  pathToSqlite <- fs::path(
    fs::path_package("tidyOhdsiRecipies"),
    "testdata",
    "test_database.sqlite"
  )
  rlang::check_installed("RSQLite")
  connectionDetails <- DatabaseConnector::createConnectionDetails(
    dbms = "sqlite", server = pathToSqlite
  )
  concept_ancestor <- data.frame(
    ancestor_concept_id = 1124300,
    descendant_concept_id = 40032408
  )
  con <- DatabaseConnector::connect(connectionDetails)
  DatabaseConnector::insertTable(
    con,
    databaseSchema = 'main',
    tableName = 'concept_ancestor',
    data = concept_ancestor
  )
  return(con)
}


addDate <- function(x, cols) {
  x <- dplyr::rowwise(dplyr::select(
    x, "person_id", "observation_period_start_date",
    "observation_period_end_date"
  ))
  for (col in cols) {
    x <- dplyr::mutate(x, diff = as.integer(difftime(
      .data$observation_period_end_date,
      .data$observation_period_start_date
    )), days = sample.int(.data$diff +
      1, 1) - 1, `:=`(!!col, .data$observation_period_start_date +
      .data$days), observation_period_start_date = .data[[col]])
  }
  x <- dplyr::select(
    dplyr::ungroup(x), -"observation_period_start_date",
    -"observation_period_end_date", -"diff", -"days"
  )
  return(x)
}
