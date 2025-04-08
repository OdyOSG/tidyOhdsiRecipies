#' Create Capr Concept Set Based Cohort
#'
#' This function creates a cohort based on a Capr concept set with detailed information.
#'
#' @param conceptSet Capr concept set (Capr S4 class) with details.
#' @param requiredObservation A vector of two integers specifying the observation period before and after the index date.
#' @param limit A character string specifying the limit of events. Possible values are "first", "all", and "last".
#' @param end A character string specifying the end strategy. Possible values are "observation_period_end_date", "drug_exit", and "fixed_exit".
#' @param endArgs A list of parameters that define the end strategy for the cohort. The list can include:
#'   \itemize{
#'     \item \code{conceptSet}: The Capr concept set used to define the cohort.
#'     \item \code{persistenceWindow}: An integer specifying the number of days after the index event during which subsequent events are considered part of the same episode.
#'     \item \code{surveillanceWindow}: An integer specifying the number of days after the persistence window during which no events should occur for the episode to be considered ended.
#'     \item \code{daysSupplyOverride}: An optional integer to override the days supply for drug exposures.
#'     \item \code{index}: A character vector specifying the index date(s) for the cohort. Possible values include "startDate" and "endDate".
#'     \item \code{offsetDays}: An integer specifying the number of days to offset the end date from the index date.
#'   }
#' @param addSourceCriteria A boolean flag indicating whether to include source criteria in the cohort definition.
#'   \itemize{
#'     \item \code{TRUE}: Source criteria will be added, which may include additional conditions or attributes to refine the cohort.
#'     \item \code{FALSE}: Source criteria will not be added, and the cohort will be defined solely based on the primary criteria.
#'   }
#'
#' @return A CirceR cohort.
#' @export
#'
#' @examples
#' \dontrun{
#' caprCs <- getCaprCsDetails(
#'   Capr::cs(list(123456, 789012), name = "Example Concept Set"), cdm
#' )
#'
#' coh <- createCaprConceptSetCohort(caprCs)
#' }
createCaprConceptSetCohort <- function(
    conceptSet,
    limit = "first",
    requiredObservation = c(0, 0),
    end = "observation_period_end_date",
    endArgs = list(
      conceptSet = conceptSet,
      persistenceWindow = 30L,
      surveillanceWindow = 0L,
      daysSupplyOverride = NULL,
      index = c("startDate"),
      offsetDays = 7
    ),
    addSourceCriteria = FALSE) {

  checkmate::assert_class(conceptSet, "ConceptSet")
  checkmate::assertIntegerish(
    requiredObservation,
    lower = 0,
    any.missing = FALSE, len = 2
  )
  limit <- snakecase::to_any_case(
    checkmate::matchArg(limit, c("first", "all", "last")),
    case = "title"
  )
  checkmate::assertList(
    endArgs,
    any.missing = TRUE,
    min.len = 0,
    max.len = 6,
    unique = TRUE,
    null.ok = TRUE
  )
  checkmate::assert(
    checkmate::check_choice(endArgs$index, choices = c("startDate", "endDate")),
    "index must be either 'startDate' or 'endDate'"
  )
  domains <- purrr::pluck(
    conceptSet, "Expression"
  ) |>
    purrr::map_chr(~ .x@Concept@domain_id) |>
    unique() |>
    tolower()

  .exit <- gsub(
    "_period_end_date", "_exit",
    checkmate::matchArg(
      end,
      c(
        "observation_period_end_date",
        "drug_exit",
        "fixed_exit"
      )
    )
  )
  .exit <- SqlRender::snakeCaseToCamelCase(
    gsub("event_end_date", "fixed_exit", .exit)
  )
  fnsCalls <- purrr::map(
    purrr::map_chr(domains, ~ glue::glue("Capr::{capr_ref(.x)}")),
    ~ prepareCall(.x, args = list(conceptSet = conceptSet))
  )
  .evals <- purrr::map_chr(
    seq_along(fnsCalls),
    ~ glue::glue("eval(fnsCalls[[{.x}]])")
  ) |>
    paste(collapse = ",")

  args <- list(
    observationWindow = prepareCall("Capr::continuousObservation", list(
      priorDays = requiredObservation[[1]],
      postDays  = requiredObservation[[2]]
    )),
    endStrategy = prepareCall(paste0("Capr::", .exit), endArgs)
  )

  cohortAttrs <- list(
    entry = eval(rlang::parse_expr(glue::glue("Capr::entry({.evals})"))),
    exit = prepareCall("Capr::exit", args)
  )

  cc <- eval(prepareCall("Capr::cohort", cohortAttrs))

  cc@entry@primaryCriteriaLimit <- limit
  cc@attrition@expressionLimit <- limit
  cc@entry@qualifiedLimit <- limit

  cohort <- Capr::toCirce(cc)
  if (addSourceCriteria) cohort <- .addSourceConceptEntry(cohort)
  return(cohort)
}

capr_ref <- function(domain_ids) {
  entry <- dplyr::tribble(
    ~domain_id, ~capr_spec,
    "condition", "conditionOccurrence",
    "drug", "drugExposure",
    "procedure", "procedure",
    "observation", "observation",
    "measurement", "measurement",
    "visit", "visit",
    "device", "deviceExposure"
  ) |>
    dplyr::filter(.data$domain_id %in% domain_ids) |>
    dplyr::pull(.data$capr_spec)
}
.addSourceConceptEntry <- function(cohort) {
  domainsN <- length(cohort$PrimaryCriteria$CriteriaList)
  domainBlocks <- purrr::map_chr(
    1:domainsN, ~ cohort$PrimaryCriteria$CriteriaList[[.x]] |>
      names() |>
      split_on_second_uppercase()
  )
  occurrences <- purrr::map_chr(
    1:domainsN, ~ cohort$PrimaryCriteria$CriteriaList[[.x]] |>
      names()
  )
  sourceCriteria <- purrr::map_chr(
    1:domainsN, ~ paste0(domainBlocks[[.x]], "SourceConcept")
  )
  for (.N in 1:domainsN) {
    cohort$PrimaryCriteria$CriteriaList[[domainsN + .N]] <-
      cohort$PrimaryCriteria$CriteriaList[[.N]]
    occurrence <- occurrences[[.N]]
    sourceCriterion <- sourceCriteria[[.N]]
    names(cohort$PrimaryCriteria$CriteriaList[[domainsN + .N]][[occurrence]]) <-
      sourceCriterion
  }
  return(cohort)
}
split_on_second_uppercase <- function(string) {
  # Find the positions of all uppercase letters
  positions <- stringr::str_locate_all(string, "[A-Z]")[[1]][, 1]

  # Check if there are at least two uppercase letters
  if (length(positions) < 2) {
    return(string)
  }

  # Split the string at the second uppercase letter
  second_uppercase_pos <- positions[2]
  part1 <- substr(string, 1, second_uppercase_pos - 1)
  part2 <- substr(string, second_uppercase_pos, nchar(string))

  return(c(part1))
}
