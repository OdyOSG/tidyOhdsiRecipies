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
    dplyr::filter(.data$concept_id %in% ids) |>
    dplyr::collect() |>
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


