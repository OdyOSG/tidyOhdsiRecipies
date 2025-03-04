test_that("Tidy CDM Referrence test", {
  numberIndividuals <- 1
  rlang::check_installed("duckdb")
  con <- duckdb::dbConnect(duckdb::duckdb(), ":memory:")
  writeSchema <- "main"
  tables <- list(
    "observation_period" = data.frame(),
    "person" = data.frame()
  )
  omopgenerics::assertList(tables, named = TRUE, class = "data.frame")
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
    if (length(id) == 1) {
      colDates <- cols[grepl("_date", cols)]
      for (i in seq_along(colDates)) {
        dates <- dplyr::union_all(dates, dplyr::tibble(
          person_id = x[[id]],
          date = as.Date(x[[colDates[i]]])
        ))
      }
    }
  }
  if ("observation_period" %in% names(tables)) {
    if (nrow(dates) == 0) {
      tables[["observation_period"]] <- dplyr::mutate(
        dplyr::select(
          tables[["person"]],
          "person_id", "year_of_birth"
        ),
        observation_period_start_date = as.Date(NA),
        observation_period_end_date = as.Date(NA)
      )
    } else {
      tables[["observation_period"]] <- dplyr::left_join(dplyr::select(
        tables[["person"]],
        "person_id", "year_of_birth"
      ), dplyr::summarise(dplyr::group_by(
        dates,
        .data$person_id
      ), observation_period_start_date = min(.data$date,
        na.rm = TRUE
      ), observation_period_end_date = max(.data$date,
        na.rm = TRUE
      )), by = "person_id")
    }
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
  }
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
  tables[["ct"]] <- dplyr::tribble(
    ~cohort_definition_id, ~subject_id, ~cohort_start_date, ~cohort_end_date,
    1, 2, as.Date("2020-01-01"), as.Date("2020-01-01")
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

  cdm <- tidyOhdsiRecipies::tidyCdmFromCon(
    con = con,
    cdmSchema = writeSchema,
    writeSchema = writeSchema,
    cohortTables = "ct",
    cdmName = "testing"
  )

  testthat::expect_s3_class(cdm, "cdm_reference")
  CDMConnector::cdmDisconnect(cdm)
})

test_that("Tidy Generate From CDM", {
  cdm <- tidyOhdsiRecipies::tidyCdmMock()
  caprCs1 <- Capr::cs(4133224, name = "lobar pneumonia")
  csWithDetails <- purrr::map(
    list(caprCs1), ~
      tidyOhdsiRecipies::getCaprCsDetails(.x, cdm)
  )
  cohortsBasedOnCs <- purrr::map(
    csWithDetails, ~ tidyOhdsiRecipies::createCaprConceptSetCohort(.x, addSourceCriteria = TRUE)
  )
  named_cohort_list <- list(
    Lobar_pneumonia = cohortsBasedOnCs[[1]]
  )
  cohortSet <- tidyOhdsiRecipies::CirceR2CDMConn(named_cohort_list)
  cdm <- tidyOhdsiRecipies::tidyGenerate(
    cdm,
    cohortSet = cohortSet,
    name = "test_cohort"
  )
  testthat::expect_true(!is.null(cdm$test_cohort))
  .res <- cdm$test_cohort |> dplyr::collect()
  testthat::expect_gt(nrow(.res), 0)
  CDMConnector::cdmDisconnect(cdm)
})

