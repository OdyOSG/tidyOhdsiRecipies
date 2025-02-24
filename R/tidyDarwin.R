#' Tidy CDM Reference From DBI Con
#'
#' This function collects and tidies OMOP CDM tables from a given database connection and schema.
#'
#' @param con A database connection DBI object.
#' @param cdmSchema The schema name where the CDM tables are located.
#' @param writeSchema The schema name where the cohort tables are located.
#' @param cohortTables A character vector of cohort table names. Default is NULL.
#' @param cdmVersion The version of the CDM. Default is NULL.
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
    cdmVersion = NULL,
    cdmName = cdmSchema) {
  src <- CDMConnector::dbSource(
    con = con,
    writeSchema = writeSchema
  )
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
  )) |> rlang::set_names(tolower(omop_tables))
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
    nms <- paste0(cohort_table, c(
      "", "_set", "_attrition",
      "_codelist"
    ))
    x <- purrr::map(nms, function(nm) {
      if (nm %in% write_schema_tables) {
        dplyr::tbl(
          src = src,
          schema = writeSchema,
          name = nm
        )
      } else if (nm %in% toupper(write_schema_tables)) {
        dplyr::tbl(
          src = src, schema = writeSchema,
          name = toupper(nm)
        )
      } else {
        NULL
      }
    })
    cdm[[cohort_table]] <- x[[1]]
    if (is.null(cdm[[cohort_table]])) {
      rlang::abort(glue::glue("cohort table `{cohort_table}` not found!"))
    }
    cdm[[cohort_table]] <- tidyNewCohortTable(
      cdm[[cohort_table]],
      cohortSetRef = x[[2]],
      cohortAttritionRef = x[[3]],
      cohortCodelistRef = x[[4]]
    )
  }
  attr(cdm, "cdm_schema") <- cdmSchema
  attr(cdm, "write_schema") <- writeSchema
  attr(cdm, "dbcon") <- attr(attr(cdm, "cdm_source"), "dbcon")
  return(cdm)
}





tidyNew <- function(
    tables,
    cdmName) {
  constructCdmReference <- getFromNamespace(
    "constructCdmReference", "omopgenerics"
  )
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
    name) {
  computeAttrition <- TRUE
  overwrite <- TRUE
  con <- CDMConnector::cdmCon(cdm)
  write_schema <- CDMConnector::cdmWriteSchema(cdm)
  prefix <- ""
  if ("cohortId" %in% names(cohortSet) && !("cohort_definition_id" %in%
    names(cohortSet))) {
    cohortSet$cohort_definition_id <- cohortSet$cohortId
  }
  if ("cohortName" %in% names(cohortSet) && !("cohort_name" %in%
    names(cohortSet))) {
    cohortSet$cohort_name <- cohortSet$cohortName
  }
  if (!("cohort" %in% names(cohortSet)) && ("json" %in% names(cohortSet))) {
    cohortColumn <- list()
    for (i in seq_len(nrow(cohortSet))) {
      x <- cohortSet$json[i]
      if (!validUTF8(x)) {
        x <- stringi::stri_enc_toutf8(x, validate = TRUE)
      }
      if (!validUTF8(x)) {
        rlang::abort("Failed to convert json UTF-8 encoding")
      }
      cohortColumn[[i]] <- jsonlite::fromJSON(x, simplifyVector = FALSE)
    }
    cohortSet$cohort <- cohortColumn
  }
  existingTables <- CDMConnector::listTables(con, write_schema)
  for (x in paste0(name, c("", "_count", "_set", "_attrition"))) {
    if (x %in% existingTables) {
      if (overwrite) {
        DBI::dbRemoveTable(con, .inSchema(
          write_schema,
          x,
          dbms = CDMConnector::dbms(con)
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
  createCohortTables <- getFromNamespace(
    "createCohortTables", "CDMConnector"
  )
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
        targetDialect = CDMConnector::dbms(con),
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
    omopgenerics::newCdmTable(cohort_ref, src = attr(
      cdm,
      "cdm_source"
    ), name = name)
  cohortSetRef <- dplyr::transmute(cohortSet,
    cohort_definition_id = as.integer(.data$cohort_definition_id),
    cohort_name = as.character(.data$cohort_name)
  )
  cdm[[name]] <- tidyNewCohortTable(
    table = cdm[[name]],
    cohortSetRef = cohortSetRef,
    cohortAttritionRef = cohort_attrition_ref
  )
  cli::cli_progress_done()
  return(cdm)
}

tidyNewCohortTable <- function(
    table,
    cohortAttritionRef,
    cohortSetRef = attr(table, "cohort_set"),
    cohortCodelistRef = attr(table, "cohort_codelist")) {
  if (!is.null(cohortSetRef)) {
    cohortSetRef <- dplyr::as_tibble(cohortSetRef)
  }
  if (!is.null(cohortAttritionRef)) {
    cohortAttritionRef <- dplyr::as_tibble(cohortAttritionRef)
  }
  if (!is.null(cohortCodelistRef)) {
    cohortCodelistRef <- dplyr::as_tibble(cohortCodelistRef)
  }
  # table <- methods::removeClass(table, "cohort_table")
  attr(table, "cohort_set") <- NULL
  attr(table, "cohort_attrition") <- NULL
  attr(table, "cohort_codelist") <- NULL

  cohortSetRef <- populateCohortSet(table, cohortSetRef)
  cohortAttritionRef <- populateCohortAttrition(
    table, cohortSetRef,
    cohortAttritionRef
  )
  cohortCodelistRef <- populateCohortCodelist(table, cohortCodelistRef)
  cohort <- constructGeneratedCohortSet(
    table = table, cohortSetRef = cohortSetRef,
    cohortAttritionRef = cohortAttritionRef, cohortCodelistRef = cohortCodelistRef
  )
  return(cohort)
}

populateCohortCodelist <- getFromNamespace(
  "populateCohortCodelist", "omopgenerics"
)

constructGeneratedCohortSet <- getFromNamespace(
  "constructGeneratedCohortSet", "omopgenerics"
)
.inSchema <- getFromNamespace(
  ".inSchema", "CDMConnector"
)
computeAttritionTable <- getFromNamespace(
  "computeAttritionTable", "CDMConnector"
)
populateCohortSet <- getFromNamespace(
  "populateCohortSet", "omopgenerics"
)
populateCohortAttrition <- getFromNamespace(
  "populateCohortAttrition", "omopgenerics"
)
.validate <- function(cdm, version = "5.3") {
  xNames <- names(cdm)
  x <- xNames[xNames != tolower(xNames)]
  omopTables <- omopgenerics::omopTables()
  omopTables <- omopTables[omopTables %in% xNames]
  for (nm in omopTables) {
    if (nm %in% c("person", "observation_period")) {
      cdm[[nm]] <- omopgenerics::newOmopTable(
        cdm[[nm]],
        version = version
      )
    } else {
      cdm[[nm]] <- tryCatch(expr = {
        omopgenerics::newOmopTable(cdm[[nm]], version = version)
      }, error = function(e) {
        cli::cli_warn(c(
          "{nm} table not included in cdm because:",
          as.character(e)
        ))
        return(NULL)
      })
    }
  }
  return(invisible(cdm))
}
