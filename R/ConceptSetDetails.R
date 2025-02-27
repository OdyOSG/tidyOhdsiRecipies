#' Get Capr Concept Set Details from cdm
#'
#' This function retrieves detailed information for a given Capr concept set or a list of concept IDs from the
#' cdm reference, analogue of `Capr::getConceptSetDetails`
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
  checkmate::assertClass(cdm, "cdm_reference")
  checkmate::assert(methods::is(x, "ConceptSet"), is.numeric(x))
  if (!methods::is(x, "ConceptSet")) {
    x <- Capr::cs(x, name = paste0(".name_", x[[1]]))
  }
  ids <- purrr::map_int(x@Expression, ~ .@Concept@concept_id)
  df <- cdm[["concept"]] |>
    dplyr::filter(.data$concept_id %in% ids) |>
    dplyr::collect() |>
    dplyr::tibble() |>
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





#' Merge Capr Concept Sets and Concept Ids
#'
#' This function merges concept sets and concepts into a single concept set.
#'
#' @param x A named list of `ConceptSet` objects.
#' @param conceptIds A vector of concept IDs to be included in the merged concept set.
#'
#' @return A `ConceptSet` object containing the merged concepts.
#' @export
#'
#' @examples
#' \dontrun{
#' conceptSet1 <- Capr::cs(conceptIds1, name = "Concept Set 1")
#' conceptSet2 <- Capr::cs(conceptIds2, name = "Concept Set 2")
#' mergedConceptSet <- mergeCsAndConcepts(list(conceptSet1, conceptSet2), conceptIds = 1:2)
#' }
mergeCsAndConcepts <- function(x, conceptIds) {
  rlang::check_installed('tidyr')
  checkmate::assertVector(conceptIds,
                          any.missing = FALSE,
                          unique = TRUE)
  checkmate::assertList(
    x,
    types = "ConceptSet",
    names = "named"
  )
  nm <- names(x)
  df <- purrr::map_dfr(
    list(purrr::chuck(x, 1), Capr::cs(conceptIds, name = nm)), .capr2Tibble
  ) |>
    dplyr::group_by(.data$concept_id) |>
    dplyr::slice(1) |>
    dplyr::ungroup()

  names(df) <- tolower(names(df))

  name <- df[["name"]][1] %||% df[["concept_set_name"]][1] %||%
    name
  if (is.na(name) || is.null(name)) {
    name <- ""
  }
  conceptDf <- dplyr::tibble(
    id = df[["concept_id"]] %||%
      df[["concept id"]] |> as.integer(), isExcluded = df[["isexcluded"]] %||%
      df[["exclude"]] %||% FALSE |> as.logical(), includeDescendants = df[["includedescendants"]] %||%
      df[["descendants"]] %||% FALSE |> as.logical(),
    includeMapped = df[["includemapped"]] %||% df[["mapped"]] %||%
      FALSE |> as.logical(), conceptName = df[["concept_name"]] %||%
      df[["concept name"]] %||% "" |> as.character(),
    standardConcept = df[["standard_concept"]] %||%
      df[["standard concept"]] %||% "" |> as.character(),
    standardConceptCaption = df[["standard_concept_caption"]] %||%
      "" |> as.character(), invalidReason = df[["invalid_reason"]] %||%
      "" |> as.character(), invalidReasonCaption = df[["invalid_reason_caption"]] %||%
      "" |> as.character(), conceptCode = df[["concept_code"]] %||%
      df[["concept code"]] %||% "" |> as.character(),
    domainId = df[["domain_id"]] %||% df[["domain"]] %||%
      "" |> as.character(), vocabularyId = df[["vocabulary_id"]] %||%
      df[["vocabulary"]] %||% "" |> as.character(),
    conceptClassId = df[["concept_class_id"]] %||% "" |>
      as.character()
  ) |> dplyr::mutate_if(
    is.character,
    ~ tidyr::replace_na(.x, "")
  )
  conceptList <- purrr::pmap(conceptDf, newConcept)
  rlang::inject(Capr::cs(!!!conceptList, name = name))
}

