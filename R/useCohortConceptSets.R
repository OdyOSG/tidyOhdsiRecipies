#' Collect Capr Concept Sets from Cohort
#'
#' This function collects concept sets from a given cohort donor and returns a named list of collection
#' Capr ConceptSet s4 class.
#'
#' @param cohortDonor A list containing a cohort donor `jsonlite::read_json` or `CirceR` output definition
#'
#' @return A named list of concept sets.
#' @export
#'
#' @examples
#' \dontrun{
#' cohortDonor <- jsonlite::read_json(fs::path(
#'   fs::path_package("tidyOhdsiRecipies"), "cohorts", "PHN.json"
#' ))
#' caprConceptSets <- tidyOhdsiRecipies::collectCaprCsFromCohort(cohortDonor)[1:2]
#' cohs <- purrr::map(
#'   caprConceptSets,
#'   ~tidyOhdsiRecipies::createCaprConceptSetCohort(.x)
#' )
#' }
collectCaprCsFromCohort <- function(cohortDonor) {

  checkmate::assertList(cohortDonor)

  .all_cs <- purrr::map(
    cohortDonor$ConceptSets, ~ purrr::pluck(.x, "expression")
  )
  .nms <- purrr::map_chr(
    cohortDonor$ConceptSets,
    ~ gsub("[^[:alnum:] ]", "", purrr::pluck(.x, "name")) |>
      snakecase::to_lower_camel_case()
  )
  caprLst <- rlang::set_names(purrr::map2(.all_cs, .nms, .getNewConceptList),.nms)

  return(caprLst)
}


.getNewConceptList <- function(.expression, .nm) {
  expression <- list()
  expression$items <- .removeItemDuplicates(
    purrr::pluck(.expression, "items")
  )
  newConcept <- getFromNamespace("newConcept", "Capr")
  conceptList <- purrr::map(expression$items, ~ newConcept(
    id = .x$concept$CONCEPT_ID,
    isExcluded = purrr::pluck(.x, "isExcluded", .default = FALSE),
    includeDescendants = purrr::pluck(.x, "includeDescendants", .default = FALSE),
    includeMapped = purrr::pluck(.x, "includeMapped", .default = FALSE),
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
