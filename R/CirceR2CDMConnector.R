#' Create Cohort Set from Named Cohort List
#'
#' This function creates a cohort set from a named list of cohorts and returns it as a `CohortSet` object.
#'
#' @param named_cohort_list A named list of CirceR cohorts.
#'
#' @return A `CohortSet` object containing the cohort definitions.
#' @export
#'
#' @examples
#' \dontrun{
#' cdm <- tidyOhdsiRecipies::tidyCdmMock()
#' caprCs1 <- Capr::cs(4133224, name = "lobar pneumonia")
#' caprCs2 <- Capr::cs(45176377, name = "velcade")
#'
#' csWithDetails <- purrr::map(
#'   list(caprCs1, caprCs2), ~
#'     tidyOhdsiRecipies::getCaprCsDetails(.x, cdm)
#' )
#' cohortsBasedOnCs <- purrr::map(
#'   csWithDetails, ~ tidyOhdsiRecipies::createCaprConceptSetCohort(.x, addSourceCriteria = TRUE)
#' )
#'
#' named_cohort_list <- list(
#'   Lobar_pneumonia = cohortsBasedOnCs[[1]],
#'   velcade = cohortsBasedOnCs[[2]]
#' )
#'
#' cohortSet <- CirceR2CDMConn(named_cohort_list)
#' }
CirceR2CDMConn <- function(named_cohort_list) {
  checkmate::assertList(
    named_cohort_list,
    min.len = 1,
    any.missing = FALSE,
    types = c("list"),
    names = "named")

  cohortsToCreate <- dplyr::tibble(
    cohort_definition_id = seq_along(named_cohort_list),
    cohort_name = names(named_cohort_list)
  ) |>
    dplyr::mutate(cohort = named_cohort_list) |>
    dplyr::mutate(json = purrr::map(
      .data$cohort,
      ~ as.character(RJSONIO::toJSON(.x, pretty = TRUE))
    )) |>
    dplyr::mutate(cohort_name = stringr::str_replace_all(
      tolower(.data$cohort_name),
      "\\s", "_"
    )) |>
    dplyr::mutate(cohort_name = stringr::str_remove_all(
      .data$cohort_name,
      "[^a-z0-9_]"
    )) |>
    dplyr::mutate(cohort_definition_id = dplyr::if_else(stringr::str_detect(
      .data$cohort_name,
      "^[0-9]+$"
    ), suppressWarnings(as.integer(.data$cohort_name)),
    .data$cohort_definition_id
    )) |>
    dplyr::mutate(cohort_name = dplyr::if_else(stringr::str_detect(
      .data$cohort_name,
      "^[0-9]+$"
    ), paste0("cohort_", .data$cohort_name),
    .data$cohort_name
    ))

  cohortsToCreate <- cohortsToCreate |>
    dplyr::mutate(cohort_name_snakecase = snakecase::to_snake_case(.data$cohort_name)) |>
    dplyr::select(
      "cohort_definition_id", "cohort_name",
      "cohort", "json", "cohort_name_snakecase"
    )
  for (i in seq_len(nrow(cohortsToCreate))) {
    first_chr <- substr(
      cohortsToCreate$cohort_name[i],
      1, 1
    )
    if (!grepl("[a-zA-Z]", first_chr)) {
      cli::cli_abort("Cohort names must start with a letter but {cohortsToCreate$cohort_name[i]} does not.\n                     Rename the json file or use a CohortsToCreate.csv file to explicitly set cohort names.")
    }
  }
  class(cohortsToCreate) <- c("CohortSet", class(cohortsToCreate))
  return(cohortsToCreate)
}
