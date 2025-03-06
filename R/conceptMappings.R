#' Get Concept Ids From Concept Codes
#'
#' This function retrieves concept IDs from a given list of concept codes and vocabularies.
#'
#' @param cdm cdm_reference via CDMConnector.
#' @param listCodes A vector of concept codes.
#' @param vocabularyIds A vector of vocabularies. Default is 'ICD10CM'.
#'
#' @return A vector of concept IDs.
#' @export
#'
#' @examples
#' \dontrun{
#' cdm <- tidyOhdsiRecipies::tidyCdmMock()
#' conceptIds <- conceptIdsFromSources(cdm, c("63020004901"), c("NDC"))
#' }
conceptIdsFromSources <- function(cdm,  listCodes, vocabularyIds = c("ICD10CM")) {
  checkmate::assertClass(cdm, "cdm_reference")
  prepCodes <- tolower(listCodes)
  prepVocab <- tolower(vocabularyIds)
  res <- cdm[["concept"]] |>
    dplyr::filter(tolower(.data$vocabulary_id) %in% prepVocab) |>
    dplyr::filter(tolower(.data$concept_code) %in% prepCodes) |>
    dplyr::select(.data$concept_id) |>
    dplyr::distinct() |>
    dplyr::pull()
  return(res)
}

#' Get Mappings for Source Concept Ids
#'
#' This function retrieves standard concept IDs that map to given source concept IDs.
#'
#' @param cdm cdm_reference via CDMConnector
#' @param sourceConceptIds A vector of source concept IDs.
#'
#' @return A vector of standard concept IDs.
#' @export
#'
#' @examples
#' \dontrun{
#' cdm <- tidyOhdsiRecipies::tidyCdmMock()
#' conceptIds <- conceptIdsFromSources(cdm, c("63020004901"), c("NDC"))
#' standardConcepts <- standardFromSourceConceptIds(cdm, conceptIds)
#' }
standardFromSourceConceptIds <- function(cdm, sourceConceptIds) {
  checkmate::assertClass(cdm, "cdm_reference")
  standardConcepts <- cdm[["concept_relationship"]] |>
    dplyr::filter(.data$concept_id_1 %in% sourceConceptIds &
      .data$relationship_id == "Maps to") |>
    dplyr::select(.data$concept_id_2) |>
    dplyr::distinct() |>
    dplyr::pull()
  return(standardConcepts)
}

#' Add Dot in ICD10 Codes
#'
#' This function adds a dot in ICD10 codes at the appropriate position.
#'
#' @param input_string An ICD10 string.
#'
#' @return A string with a dot added at the appropriate position.
#' @export
#'
#' @examples
#' \dontrun{
#' icd10_with_dot <- addDot("A001")
#' }
addDot <- function(input_string) {
  if (nchar(input_string) >= 4) {
    return(paste0(
      substr(input_string, 1, 3), ".",
      substr(input_string, 4, nchar(input_string))
    ))
  } else {
    return(input_string)
  }
}

#' Get Descendant Concepts
#'
#' This function retrieves the descendant concept IDs for a given set of ancestor concept IDs from a specified vocabulary database schema.
#'
#' @param x A vector of ancestor concept IDs.
#' @param con A valid database connection object.
#' @param vocabularyDatabaseSchema A character string specifying the schema of the vocabulary database.
#'
#' @return A vector of descendant concept IDs, including the original ancestor concept IDs.
#' @export
#'
#' @examples
#' \dontrun{
#' con <- DBI::dbConnect(RPostgres::Postgres(), dbname = "your_database")
#' ancestor_ids <- c(21164796, 21164797)
#' vocabulary_schema <- "cdm5"
#' getDescendants(ancestor_ids, con, vocabulary_schema)
#' }
getDescendants <- function(x, con, vocabularyDatabaseSchema) {
  checkmate::assert(DBI::dbIsValid(con))
  checkmate::assert_vector(x)
  checkmate::assert_character(vocabularyDatabaseSchema)
  sql <- SqlRender::render(
    "select distinct descendant_concept_id
     from @vocab_schema.concept_ancestor
     where ancestor_concept_id IN (@x)",
    vocab_schema = vocabularyDatabaseSchema,
    x = x
  ) |> SqlRender::translate(targetDialect = dbms(con))
  desc <- suppressWarnings(DBI::dbGetQuery(con, sql, immediate = TRUE)) |>
    dplyr::pull(.data$descendant_concept_id)
  return(dplyr::union(desc, x))
}
