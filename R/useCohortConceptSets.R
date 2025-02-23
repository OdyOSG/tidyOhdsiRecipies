#' Collect Capr Concept Sets from Cohort
#'
#' This function collects concept sets from a given cohort donor and returns a named list.
#'
#' @param cohortDonor A list containing a cohort donor `jsonlite::read_json` output definition
#'
#' @return A named list of concept sets.
#' @export
#'
#' @examples
#' \dontrun{
#' cohortDonor <- jsonlite::read_json("inst/cohorts/ToGet/Male.json")
#' caprConceptSets <- collectCaprCsFromCohort(cohortDonor)
#' }
collectCaprCsFromCohort <- function(cohortDonor) {
  .all_cs <- map(
    cohortDonor$ConceptSets, ~ pluck(.x, "expression")
  )
  .nms <- map_chr(
    cohortDonor$ConceptSets,
    ~ gsub("[^[:alnum:] ]", "", pluck(.x, "name")) |>
      snakecase::to_lower_camel_case()
  )
  caprLst <- map2(.all_cs, .nms, .getNewConceptList) |>
    rlang::set_names(.nms)
  return(caprLst)
}


.getNewConceptList <- function(.expression, .nm) {
  expression <- list()
  expression$items <- .removeItemDuplicates(
    pluck(.expression, "items")
  )
  newConcept <- getFromNamespace("newConcept", "Capr")
  conceptList <- map(expression$items, ~ newConcept(
    id = .x$concept$CONCEPT_ID,
    isExcluded = pluck(.x, "isExcluded", .default = FALSE),
    includeDescendants = pluck(.x, "includeDescendants", .default = FALSE),
    includeMapped = pluck(.x, "includeMapped", .default = FALSE),
    conceptName = .x$concept$CONCEPT_NAME, standardConcept = .x$concept$STANDARD_CONCEPT,
    standardConceptCaption = .x$concept$STANDARD_CONCEPT_CAPTION,
    invalidReason = .x$concept$INVALID_REASON, conceptCode = .x$concept$CONCEPT_CODE,
    domainId = .x$concept$DOMAIN_ID, vocabularyId = .x$concept$VOCABULARY_ID,
    conceptClassId = .x$concept$CONCEPT_CLASS_ID
  ))
  rlang::inject(Capr::cs(!!!conceptList, name = .nm))
}

.removeItemDuplicates <- function(items) {
  uniqueItems <- list()
  seenIds <- c()
  for (item in items) {
    concept_id <- item$concept$CONCEPT_ID
    if (!(concept_id %in% seenIds)) {
      uniqueItems <- append(uniqueItems, list(item))
      seenIds <- c(seenIds, concept_id)
    }
  }
  return(uniqueItems)
}
