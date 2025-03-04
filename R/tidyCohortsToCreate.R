#' Create Cohorts To Create Tibble For `CohortGenerator`
#'
#' This function creates a tibble for the `CohortGenerator` package from cohort JSON files or a list of `CirceR` cohorts.
#'
#' @param path A character string specifying the path to the cohort JSON files.
#' @param computeAttrition A boolean indicating whether to compute attrition statistics.
#' @param cohorts A named list of `CirceR` cohorts.
#'
#' @return A tibble for the `CohortGenerator` package.
#' @export
#'
#' @examples
#' \dontrun{
#' ff <- list.files("inst/concept_sets",
#'   pattern = "cs_",
#'   full.names = TRUE
#' )
#' conceptSetList <- map(ff, ~ Capr::readConceptSet(
#'   .x,
#'   name = fs::path_file(.x) |>
#'     fs::path_ext_remove()
#' ))
#'
#' cohorts <- map(conceptSetList, ~ createCaprCohort(.x)) |>
#'   rlang::set_names(
#'     ff |> fs::path_file() |>
#'       fs::path_ext_remove()
#'   )
#'
#'  generationSet <-createCohortDefinitionSet(cohorts = cohorts)
#' }
#'
createCohortDefinitionSet <- function(
    path = NULL,
    cohorts = NULL,
    computeAttrition = TRUE) {
  checkmate::assert(is.null(cohorts), is.null(path))

  checkmate::assert(!is.null(cohorts), !is.null(path))

  if (!is.null(path)) {
    jsonFiles <- sort(
      fs::dir_ls(
        path,
        regexp = "\\.json$"
      )
    )
    cohortsToCreate <- dplyr::tibble(
      cohortId = seq_along(jsonFiles),
      cohortName = fs::path_file(jsonFiles) |>
        fs::path_ext_remove(),
      json = purrr::map_chr(
        jsonFiles, readr::read_file
      )
    ) |>
      dplyr::mutate(
        sql = purrr::map_chr(
          .data$json, ~ CirceR::buildCohortQuery(
            CirceR::cohortExpressionFromJson(.x),
            CirceR::createGenerateOptions(generateStats = computeAttrition)
          )
        )
      )
  } else {
    cohortsToCreate <- dplyr::tibble(
      cohortId = seq_along(cohorts),
      cohortName = names(cohorts),
      json = purrr::map_chr(cohorts, ~ as.character(
        RJSONIO::toJSON(.x, pretty = TRUE)
      ))
    ) |>
      dplyr::mutate(
        sql = purrr::map_chr(
          .data$json, ~ CirceR::buildCohortQuery(
            CirceR::cohortExpressionFromJson(.x),
            CirceR::createGenerateOptions(generateStats = computeAttrition)
          )
        )
      )
  }
  return(cohortsToCreate)
}


#' Write Cohort From List Expression
#'
#' This function writes a cohort list expression to a specified path in JSON format.
#'
#' @param cohortName name of the cohort
#' @param saveLocation path to put cohort
#' @param cohort A CirceR list expression of the cohort.
#'
#' @return NULL
#' @export
#'
#' @examples
#' \dontrun{
#' writeListCohort(cohort, "any_cohort", "cohorts")
#' }
writeListCohort <- function(cohort, cohortName, saveLocation) {
  write(
    RJSONIO::toJSON(cohort, pretty = TRUE),
    fs::path(saveLocation, cohortName, ext = "json")
  )

  invisible(NULL)
}
