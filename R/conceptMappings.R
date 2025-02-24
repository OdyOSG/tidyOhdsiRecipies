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
conceptIdsFromSources <- function(
    cdm,
    listCodes,
    vocabularyIds = c("ICD10CM")) {
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
standardFromSourceConceptIds <- function(
    cdm, sourceConceptIds) {
  standardConcepts <- cdm[["concept_relationship"]] |>
    dplyr::filter(.data$concept_id_1 %in% sourceConceptIds &
      .data$relationship_id == "Maps to") |>
    dplyr::select(.data$concept_id_2) |>
    dplyr::distinct() |>
    dplyr::pull(.data$concept_id_2)
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
