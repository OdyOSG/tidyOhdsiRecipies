#' Tidy CDM Reference From DBI Con
#'
#' This function collects and tidies OMOP CDM tables from a given database connection and schema.
#'
#' @param con A database connection DBI object.
#' @param cdmSchema The schema name where the CDM tables are located.
#' @param writeSchema The schema name where the cohort tables are located.
#' @param cohortTables A character vector of cohort table names. Default is NULL.
#' @param cdmName The name of the CDM. Default is the same as `cdmSchema`.
#'
#' @return A list of tidied CDM tables.
#' @export
#'
#' @examples
#' \dontrun{
#' con <- DBI::dbConnect(RPostgres::Postgres(), dbname = "your_database")
#' cdmSchema <- "cdm_schema"
#' writeSchema <- "write_schema"
#' cohortTables <- c("cohort1", "cohort2")
#' cdm <- tidyCdm(con, cdmSchema, writeSchema, cohortTables)
#' }
tidyCdmFromCon <- function(
    con,
    cdmSchema,
    writeSchema,
    cohortTables = NULL,
    cdmName = "db_id") {

  checkmate::assertTRUE(DBI::dbIsValid(con))

  checkmate::assertCharacter(
    cdmName,
    any.missing = FALSE,
    len = 1, null.ok = TRUE
  )
  checkmate::assertCharacter(
    cdmSchema,
    min.len = 1, max.len = 3,
    any.missing = F
  )
  checkmate::assertCharacter(
    writeSchema,
    min.len = 1, max.len = 3,
    any.missing = F
  )
  checkmate::assertCharacter(
    cohortTables,
    null.ok = TRUE,
    min.len = 1
  )

  if (dbms(con) == "sqlite") {
    cli::cli_abort("SQLite is not supported by CDMConnector. Please use duckdb instead.")
  }
  if (methods::is(con, "DatatbaseConnectorConnection") &&
    dbms(con) != "postgresql") {
    cli::cli_warn("DatabaseConnector connections on {dbms(con)} are not tested!")
  }
  if (dbms(con) %in% c("oracle")) {
    cli::cli_warn("Oracle database connections are not tested!")
  }
  if (missing(writeSchema)) {
    cli::cli_abort("{.arg writeSchema} is now required to create a cdm object with a database backend.\n                   Please make sure you have a schema in your database where you can create new tables and provide it in the `write_schema` argument.\n                   If your schema has multiple parts please provide a length 2 character vector: `write_schema = c('my_db', 'my_schema')`")
  }

  src <- CDMConnector::dbSource(con, writeSchema)

  con <- attr(src, "dbcon")

  dbTables <- CDMConnector::listTables(con, schema = cdmSchema)
  omop_tables <- omopgenerics::omopTables()
  omop_tables <- omop_tables[which(omop_tables %in% tolower(dbTables))]
  cdm_tables_in_db <- dbTables[which(tolower(dbTables) %in%
    omop_tables)]
  cdmTables <- purrr::map(omop_tables, ~ dplyr::tbl(
    src = src,
    schema = cdmSchema,
    name = .x
  )) |> rlang::set_names(omop_tables)
  cdm <- tidyNew(
    tables = c(cdmTables),
    cdmName = cdmName
  )
  attr(cdm, "temp_emulation_prefix") <- paste0(
    "temp", Sys.getpid() +
      stats::rpois(1, as.integer(Sys.time())) %% 1e+06, "_"
  )
  write_schema_tables <- CDMConnector::listTables(con, schema = writeSchema)
  for (cohort_table in cohortTables) {
    if (cohort_table %in% write_schema_tables) {
      cdm <- CDMConnector::readSourceTable(cdm, cohort_table)
      cdm[[cohort_table]] <- omopgenerics::newCohortTable(
        table = cdm[[cohort_table]],
        .softValidation = TRUE
      )
    }
  }
  attr(cdm, "cdm_schema") <- cdmSchema
  attr(cdm, "write_schema") <- writeSchema
  attr(cdm, "dbcon") <- attr(attr(cdm, "cdm_source"), "dbcon")
  return(cdm)
}





tidyNew <- function(
    tables,
    cdmName) {
  constructCdmReference <- getFromNamespace("constructCdmReference", "omopgenerics")
  cdm <- constructCdmReference(
    tables = tables,
    cdmName = cdmName,
    cdmVersion = "5.3",
    cdmSource = omopgenerics::tableSource(tables[[1]])
  )
  cdm <- .validate(cdm)
  return(cdm)
}




#' Generate Cohorts from CDM
#'
#' This function generates cohorts from a given CDM and cohort set, and writes the results to the specified schema.
#'
#' @param cdm cdm_reference via CDMConnector.
#' @param cohortSet A data frame containing the cohort definitions, including JSON expressions. `CDMConnector::readCohortSet` output
#' @param name The base name for the cohort tables.
#' @param computeAttrition boolean. Should attrition be calculated?
#' @param overwrite boolean.  Should table be overwritten?
#'
#' @return A list of generated cohorts.
#' @export
#'
#' @examples
#' \dontrun{
#' cdm <- CDMConnector::cdmFromCon(con, cdmSchema = "cdm_schema")
#' cohortSet <- data.frame(
#'   cohortId = 1:2,
#'   cohortName = c("Cohort 1", "Cohort 2"),
#'   json = c("{...}", "{...}")
#' )
#' generatedCohorts <- tidyGenerate(cdm, cohortSet, "cohort_table_name")
#' }
#'
tidyGenerate <- function(
    cdm,
    cohortSet,
    name,
    computeAttrition = TRUE,
    overwrite = TRUE) {
  rlang::check_installed("withr")
  if (!is.data.frame(cohortSet)) {
    rlang::abort("`cohortSet` must be a dataframe from the output of `readCohortSet()`.")
  }
  checkmate::assertDataFrame(cohortSet, min.rows = 1, col.names = "named")
  stopifnot(all(c(
    "cohort_definition_id", "cohort_name", "cohort",
    "json"
  ) %in% names(cohortSet)))
  withr::local_options(list(cli.progress_show_after = 0, cli.progress_clear = FALSE))
  con <- CDMConnector::cdmCon(cdm)
  checkmate::assertTRUE(DBI::dbIsValid(con))
  checkmate::assertCharacter(name,
    len = 1, min.chars = 1,
    any.missing = FALSE
  )
  if (name != tolower(name)) {
    rlang::abort("Cohort table name {name} must be lowercase!")
  }
  if (!grepl("^[a-z]", substr(name, 1, 1))) {
    cli::cli_abort("Cohort table name {name} must start with a letter!")
  }
  if (!grepl("^[a-z][a-z0-9_]*$", name)) {
    cli::cli_abort("Cohort table name {name} must only contain letters, numbers, and underscores!")
  }
  checkmate::assertLogical(computeAttrition, len = 1)
  checkmate::assertLogical(overwrite, len = 1)

  write_schema <- CDMConnector::cdmWriteSchema(cdm)
  prefix <- ""
  existingTables <- CDMConnector::listTables(con, write_schema)
  for (x in paste0(name, c("", "_count", "_set", "_attrition"))) {
    if (x %in% existingTables) {
      if (overwrite) {
        DBI::dbRemoveTable(con, .inSchema(
          write_schema,
          x,
          dbms = dbms(con)
        ))
      } else {
        cli::cli_abort("The cohort table {paste0(prefix, name)} already exists.\nSpecify overwrite = TRUE to overwrite it.")
      }
    }
  }
  cohortSet$sql <- character(nrow(cohortSet))
  for (i in seq_len(nrow(cohortSet))) {
    cohortJson <- cohortSet$json[[i]]
    cohortExpression <- CirceR::cohortExpressionFromJson(expressionJson = cohortJson)
    cohortSql <- CirceR::buildCohortQuery(
      expression = cohortExpression,
      options = CirceR::createGenerateOptions(generateStats = computeAttrition)
    )
    cohortSet$sql[i] <- SqlRender::render(cohortSql, warnOnMissingParameters = FALSE)
  }

  createCohortTables <- getFromNamespace("createCohortTables", "CDMConnector")
  createCohortTables(con, write_schema, name, computeAttrition)
  cdm_schema <- attr(cdm, "cdm_schema")
  cdm_schema_sql <- glue::glue_sql_collapse(DBI::dbQuoteIdentifier(
    con,
    cdm_schema
  ), sep = ".")

  if ("prefix" %in% names(write_schema)) {
    write_schema_sql <- paste(
      DBI::dbQuoteIdentifier(
        con,
        write_schema[-which(names(write_schema) == "prefix")]
      ),
      collapse = "."
    )
  } else {
    write_schema_sql <- paste(DBI::dbQuoteIdentifier(
      con,
      write_schema
    ), collapse = ".")
  }
  generate <- function(i) {
    pct <- ""
    cli::cli_progress_step("Generating cohort ({i}/{nrow(cohortSet)}{pct}) - {cohortSet$cohort_name[i]}",
      spinner = interactive()
    )
    sql <- cohortSet$sql[i] |> SqlRender::render(
      cdm_database_schema = cdm_schema_sql,
      vocabulary_database_schema = cdm_schema_sql, target_database_schema = write_schema_sql,
      results_database_schema.cohort_inclusion = paste0(
        write_schema_sql,
        ".", DBI::dbQuoteIdentifier(con, paste0(
          prefix,
          name, "_inclusion"
        ))
      ), results_database_schema.cohort_inclusion_result = paste0(
        write_schema_sql,
        ".", DBI::dbQuoteIdentifier(con, paste0(
          prefix,
          name, "_inclusion_result"
        ))
      ), results_database_schema.cohort_summary_stats = paste0(
        write_schema_sql,
        ".", DBI::dbQuoteIdentifier(con, paste0(
          prefix,
          name, "_summary_stats"
        ))
      ), results_database_schema.cohort_censor_stats = paste0(
        write_schema_sql,
        ".", DBI::dbQuoteIdentifier(con, paste0(
          prefix,
          name, "_censor_stats"
        ))
      ), results_database_schema.cohort_inclusion = paste0(
        write_schema_sql,
        ".", DBI::dbQuoteIdentifier(con, paste0(
          prefix,
          name, "_inclusion"
        ))
      ), target_cohort_table = DBI::dbQuoteIdentifier(
        con,
        paste0(prefix, name)
      ), target_cohort_id = cohortSet$cohort_definition_id[i],
      warnOnMissingParameters = FALSE
    )

    quoteSymbol <- substr(as.character(DBI::dbQuoteIdentifier(
      con,
      "a"
    )), 1, 1)
    sql <- stringr::str_replace_all(sql, paste0(
      "_inclusion",
      quoteSymbol, "_stats"
    ), paste0(
      "_inclusion_stats",
      quoteSymbol
    ))
    stopifnot(length(unique(stringr::str_extract_all(
      sql,
      "@\\w+"
    ))[[1]]) == 0)
    sql <- stringr::str_replace_all(
      sql, "--([^\n])*?\n",
      "\n"
    )
    if (dbms(con) != "spark" && dbms(con) != "bigquery") {
      sql <- SqlRender::translate(sql,
        targetDialect = CDMConnector::dbms(con),
        tempEmulationSchema = "SQL ERROR"
      )
      if (stringr::str_detect(sql, "SQL ERROR")) {
        cli::cli_abort("sqlRenderTempEmulationSchema being used for cohort generation!\n        Please open a github issue at {.url https://github.com/darwin-eu/CDMConnector/issues} with your cohort definition.")
      }
    } else {
      if ("schema" %in% names(write_schema)) {
        s <- unname(write_schema["schema"])
      } else if (length(write_schema) == 1) {
        s <- unname(write_schema)
      } else {
        s <- unname(write_schema[2])
      }
      sql <- SqlRender::translate(sql,
        targetDialect = dbms(con),
        tempEmulationSchema = s
      )
    }
    if (dbms(con) == "duckdb") {
      sql <- gsub(
        "'-1 \\* (\\d+) day'", "'-\\1 day'",
        sql
      )
    }
    if (dbms(con) == "spark") {
      sql <- stringr::str_replace_all(
        sql, "date_add",
        "dateadd"
      )
      sql <- stringr::str_replace_all(
        sql, "DATE_ADD",
        "DATEADD"
      )
    }
    sql <- stringr::str_replace_all(sql, "\\s+", " ")
    sql <- stringr::str_split(sql, ";")[[1]] |>
      stringr::str_trim() |>
      stringr::str_c(";") |>
      stringr::str_subset("^;$",
        negate = TRUE
      )
    drop_statements <- c(
      stringr::str_subset(sql, "DROP TABLE") |>
        stringr::str_subset("IF EXISTS", negate = TRUE) |>
        stringr::str_replace("DROP TABLE", "DROP TABLE IF EXISTS"),
      stringr::str_subset(sql, "DROP TABLE IF EXISTS")
    ) |>
      purrr::map_chr(~ SqlRender::translate(.x, dbms(con)))
    drop_statements <- stringr::str_replace_all(
      drop_statements,
      "--([^\n])*?\n", "\n"
    )
    for (k in seq_along(drop_statements)) {
      if (grepl("^--", drop_statements[k])) {
        next
      }
      suppressMessages(DBI::dbExecute(con, drop_statements[k],
        immediate = TRUE
      ))
    }
    sql <- stringr::str_replace_all(
      sql, "--([^\n])*?\n",
      "\n"
    )
    for (k in seq_along(sql)) {
      if (grepl("^--", sql[k])) {
        next
      }
      DBI::dbExecute(con, sql[k], immediate = TRUE)
      if (interactive()) {
        pct <- ifelse(k == length(sql), "", glue::glue(" ~ {floor(100*k/length(sql))}%"))
        cli::cli_progress_update()
      }
    }
  }
  for (i in seq_len(nrow(cohortSet))) {
    generate(i)
  }
  cohort_ref <- dplyr::tbl(con, .inSchema(write_schema, name,
    dbms = dbms(con)
  ))
  if (computeAttrition) {
    cohort_attrition_ref <- dplyr::collect(
      computeAttritionTable(
        cdm = cdm,
        cohortStem = name,
        cohortSet = cohortSet,
        overwrite = overwrite
      )
    )
  } else {
    cohort_attrition_ref <- NULL
  }
  cdm[[name]] <-
    omopgenerics::newCdmTable(
      cohort_ref,
      src = attr(
        cdm,
        "cdm_source"
      ), name = name
    )
  cohortSetRef <- dplyr::transmute(cohortSet,
    cohort_definition_id = as.integer(.data$cohort_definition_id),
    cohort_name = as.character(.data$cohort_name)
  )
  cdm[[name]] <- omopgenerics::newCohortTable(
    table = cdm[[name]],
    cohortSetRef = cohortSetRef,
    cohortAttritionRef = cohort_attrition_ref,
    cohortCodelistRef = NULL,
    .softValidation = TRUE
  )

  cli::cli_progress_done()
  return(cdm)
}

.inSchema <- getFromNamespace(
  ".inSchema", "CDMConnector"
)
computeAttritionTable <- getFromNamespace(
  "computeAttritionTable", "CDMConnector"
)
populateCohortAttrition <- getFromNamespace(
  "populateCohortAttrition", "omopgenerics"
)
.validate <- function(cdm) {
  xNames <- names(cdm)
  omopTables <- omopgenerics::omopTables()
  omopTables <- dplyr::intersect(omopTables, xNames)
  for (.n in omopTables) {
    cdm[[.n]] <- omopgenerics::newOmopTable(cdm[[.n]])
  }
  return(invisible(cdm))
}
