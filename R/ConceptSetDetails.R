#' Get Capr Concept Set Details from cdm
#'
#' This function retrieves detailed information for a given Capr concept set or a list of concept IDs from the Common Data Model (CDM).
#'
#' @param x A list of concept IDs or a Capr concept set (Capr S4 class).
#' @param cdm cdm_reference via CDMConnector.
#'
#' @return A Capr concept set with detailed information populated from the CDM.
#' @export
#'
#' @examples
#' \dontrun{
#' # Example with a vector of concept IDs
#' concept_ids <- c(123456, 789012)
#' cdm <- CDMConnector::cdmFromCon(con, "cdm_schema", "work_schema")
#' detailed_concept_set <- getCaprCsDetails(concept_ids, cdm)
#'
#' # Example with a Capr concept set
#' capr_concept_set <- Capr::cs(concept_ids, name = "Example Concept Set")
#' detailed_concept_set <- getCaprCsDetails(capr_concept_set, cdm)
#' }
getCaprCsDetails <- function(x, cdm) {
  if (!methods::is(x, "ConceptSet")) {
    x <- Capr::cs(x, name = paste0(".name_", x[[1]]))
  }
  ids <- purrr::map_int(x@Expression, ~ .@Concept@concept_id)
  df <- cdm[["concept"]] |>
    dplyr::collect(
      dplyr::filter(.data$concept_id %in% ids)
    ) |>
    tibble::tibble() |>
    dplyr::mutate(
      invalid_reason = ifelse(is.na(.data$invalid_reason), "V", .data$invalid_reason)
    ) |>
    dplyr::mutate(
      standard_concept_caption = dplyr::case_when(
        .data$standard_concept == "S" ~ "Standard",
        .data$standard_concept == "N" ~ "Non-Standard",
        .data$standard_concept == "C" ~ "Classification",
        TRUE ~ ""
      )
    ) |>
    dplyr::mutate(
      invalid_reason_caption = dplyr::case_when(
        .data$invalid_reason == "V" ~ "Valid",
        .data$invalid_reason == "I" ~ "Invalid",
        TRUE ~ ""
      )
    )

  checkSlotNames <- methods::slotNames("Concept")[-1]
  for (i in seq_along(x@Expression)) {
    id <- x@Expression[[i]]@Concept@concept_id
    for (n in checkSlotNames) {
      dtl <- dplyr::filter(df, .data$concept_id == id) |>
        dplyr::pull(!!rlang::sym(n))
      if (length(dtl) > 0) {
        methods::slot(x@Expression[[i]]@Concept, n) <- dtl
      }
    }
  }
  return(x)
}


#' Tidy Fill in Concept Set details using a vocab
#'
#' Concept sets created in R using the `cs` function do not contain details like
#' "CONCEPT_NAME", "DOMAIN_ID", etc. If an OMOP CDM vocabulary is available then
#' these details can be filled in by the the `getConceptSetDetails` function.
#' Grabed from `https://github.com/OHDSI/Capr` the only difference - added source spec
#'
#' @param x A concept set created by `cs`
#' @param con A connection to an OMOP CDM database
#' @param vocabularyDatabaseSchema   Schema name where your OMOP vocabulary format resides.
#' @return A modified version of the input concept set with concept details filled in.
#'
#' @importFrom methods slot<-
#' @export
#'
#' @examples
#' \dontrun{
#' # create a concept set
#' vocabularyDatabaseSchema <- "main"
#' Pneumonia <- cs(4133224, name = "Pneumonia")
#'
#' # fill in the details from an OMOP CDM
#' con <- returnSqLiteDatabaseConnectorCon()
#' Pneumonia <- getConceptSetDetails(Pneumonia, con, vocabularyDatabaseSchema = "cdm5")
#' }
tidyCaprConceptSetDetails <- function(x, con, vocabularyDatabaseSchema = NULL) {
  checkmate::assertClass(x, "ConceptSet")
  checkmate::assertTRUE(DBI::dbIsValid(con))
  checkmate::assertCharacter(vocabularyDatabaseSchema,
    len = 1,
    null.ok = TRUE
  )
  ids <- purrr::map_int(x@Expression, ~ .@Concept@concept_id)
  sql <- "SELECT * FROM @schema.concept WHERE concept_id IN (@ids);" |>
    SqlRender::render(
      schema = vocabularyDatabaseSchema,
      ids = ids
    ) |>
    SqlRender::translate(targetDialect = DatabaseConnector::dbms(con))
  df <- DBI::dbGetQuery(con, sql) |>
    tibble::tibble() |>
    dplyr::rename_all(tolower) |>
    dplyr::mutate(invalid_reason = ifelse(
      is.na(.data$invalid_reason),
      "V", .data$invalid_reason
    )) |>
    dplyr::mutate(standard_concept_caption = dplyr::case_when(
      standard_concept == "S" ~ "Standard",
      standard_concept == "N" ~ "Non-Standard",
      standard_concept == "C" ~ "Classification", TRUE ~ ""
    )) |>
    dplyr::mutate(invalid_reason_caption = dplyr::case_when(
      invalid_reason ==
        "V" ~ "Valid", invalid_reason == "I" ~ "Invalid",
      TRUE ~ ""
    ))
  checkSlotNames <- methods::slotNames("Concept")[-1]
  for (i in seq_along(x@Expression)) {
    id <- x@Expression[[i]]@Concept@concept_id
    for (n in checkSlotNames) {
      dtl <- dplyr::filter(df, .data$concept_id == id) |>
        dplyr::pull(!!n)
      if (length(dtl > 0)) {
        methods::slot(x@Expression[[i]]@Concept, n) <- dtl
      }
    }
  }
  return(x)
}
