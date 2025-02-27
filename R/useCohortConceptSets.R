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
#'   ~ tidyOhdsiRecipies::createCaprConceptSetCohort(.x)
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
  caprLst <- rlang::set_names(purrr::map2(.all_cs, .nms, .getNewConceptList), .nms)

  return(caprLst)
}

#' Inject Items into a Cohort
#'
#' This function injects items from a given ConceptSet into a specified position within a cohort's ConceptSets.
#'
#' @param cohort A list representing the cohort, which contains ConceptSets.
#' @param caprCs An object of class `ConceptSet` containing the items to be injected.
#' @param position A numeric value indicating the position within the cohort's ConceptSets where the items should be injected. Must be between 1 and the length of the ConceptSets.
#' @param writeCohortPath An optional character string specifying the file path to write the modified cohort as a JSON file. Must end with `.json`.
#'
#' @return The modified cohort with the injected items.
#' @export
#'
#' @examples
#' # Example usage:
#' cohortDonor <- jsonlite::read_json(fs::path(
#' fs::path_package("tidyOhdsiRecipies"), "cohorts", "PHN.json"
#' ))
#' caprConceptSets <- tidyOhdsiRecipies::collectCaprCsFromCohort(cohortDonor)[[1]]
#' modifiedCohort <- injectItemsIntoCohort(cohortDonor, caprConceptSets, 2)
#'
injectItemsIntoCohort <- function(cohort, caprCs, position,
                                  writeCohortPath = NULL) {
  csLength <- length(cohort$ConceptSets)
  checkmate::assert_class(caprCs, "ConceptSet")
  checkmate::assert_double(position,
                           lower = 1,
                           upper = csLength,
  )
  cohort$ConceptSets[[position]]$expression$items <-
    list(items = lapply(caprCs@Expression, as.list))$items
  if (!is.null(writeCohortPath)) {
    checkmate::assertCharacter(writeCohortPath,
                               len = 1, min.chars = 1,
                               pattern = "\\.json"
    )
    writeListCohort(cohort, writeCohortPath)
  }
  return(cohort)
}




.getNewConceptList <- function(.expression, .nm) {
  expression <- list()
  expression$items <- .removeItemDuplicates(
    purrr::pluck(.expression, "items")
  )
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



as.list <- getFromNamespace("as.list", "Capr")
newConcept <- getFromNamespace("newConcept", "Capr")
