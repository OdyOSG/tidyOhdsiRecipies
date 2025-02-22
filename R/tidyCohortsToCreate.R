#' Create Cohorts To Create Tibble
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
#'   ff <- list.files("inst/concept_sets",
#'     pattern = "cs_",
#'     full.names = TRUE
#'   )
#'   conceptSetList <- map(ff, ~ Capr::readConceptSet(
#'     .x,
#'     name = fs::path_file(.x) |>
#'       fs::path_ext_remove()
#'   ))
#'
#'   cohorts <- map(conceptSetList, ~ createCaprCohort(.x)) |>
#'     rlang::set_names(
#'       ff |> fs::path_file() |>
#'         fs::path_ext_remove()
#'     )
#' }
createCohortsToCreate <- function(
    path = NULL,
    cohorts = NULL,
    computeAttrition = TRUE) {

  if (!is.null(path)) {
    jsonFiles <- sort(
      list.files(path,
                 pattern = "\\.json$",
                 full.names = TRUE
      )
    )
    cohortsToCreate <- dplyr::tibble(
      cohortId = seq_along(jsonFiles),
      cohortName = fs::path_file(jsonFiles) |>
        fs::path_ext_remove(),
      json = map_chr(
        jsonFiles, readr::read_file
      )
    ) |>
      dplyr::mutate(
        sql = map_chr(
          json, ~ CirceR::buildCohortQuery(
            CirceR::cohortExpressionFromJson(.x),
            CirceR::createGenerateOptions(generateStats = computeAttrition)
          )
        )
      )
  } else {
    cohortsToCreate <- dplyr::tibble(
      cohortId = seq_along(cohorts),
      cohortName = names(cohorts),
      json = map_chr(cohorts, ~ as.character(
        RJSONIO::toJSON(.x, pretty = TRUE)
      ))
    ) |>
      dplyr::mutate(
        sql = map_chr(
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
#' @param cohort A list expression of the cohort.
#' @param path A character string specifying the path to save the cohort JSON file.
#'
#' @return NULL
#' @export
#'
#' @examples
#' \dontrun{
#'   writeListCohort(cohort, "path/to/cohort.json")
#' }
writeListCohort <- function(cohort, path) {
  write(as.character(RJSONIO::toJSON(cohort, pretty = TRUE)), path)
  invisible(NULL)
}

utils::globalVariables(c("json"))


