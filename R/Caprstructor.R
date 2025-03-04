#' Write Capr Cohort Using Constructor
#'
#' The function allows you to build a template Capr
#' cohort using a set of arguments.
#' Most often, difficulties with using Capr arise
#' due to a lack of understanding of the
#' structure and the problem of “where to start”
#' Therefore, `writeCaprstructor` allows you to get a
#' structure with the necessary fields.
#'
#' @param entryDomains A character vector specifying the domains for entry events. Default is c('drug', 'condition').
#' @param firstTime A logical value indicating if the entry event is the first time. Default is FALSE.
#' @param attritionDomains A character vector specifying the domains for attrition events. Default is c('drug', 'condition'). Might be NULL if no attrition required
#' @param age An integer specifying the age for the entry event and it's defined as greater or equal the age
#' @param gender A character string specifying the gender for the entry event. Default is "male".
#' @param limit A character string specifying the limit of events. Possible values are "first", "all", and "last".
#' @param exitStrategy A character string specifying the end strategy. Possible values are "observation_period_end_date", "drug_exit" and "fixed_exit".
#' @param requiredObservation A vector of two integers specifying the observation period before and after the index date.
#' @param copyToClipboard boolean, if TRUE will copy  to clipboard cohort definition
#'
#' @return The function returns a Capr cohort vertical data class
#' @export
#'
#' @examples
#' \dontrun{
#' writeCaprstructor(
#'   entryDomains = c("drug", "procedure"),
#'   firstTime = TRUE,
#'   attritionDomains = c("drug", "condition"),
#'   limit = "first",
#'   age = 18,
#'   gender = "male",
#'   exitStrategy = "observation_period_end_date"
#' )
#' }
writeCaprstructor <- function(
    entryDomains = c("drug", "condition"),
    firstTime = FALSE,
    requiredObservation = c(0, 0),
    attritionDomains = c("drug", "condition"),
    limit = "first",
    age = 18,
    gender = "male",
    exitStrategy = "observation_period_end_date",
    copyToClipboard = FALSE) {
  rlang::check_installed("usethis")
  checkmate::assertChoice(
    exitStrategy,
    c("observation_period_end_date", "fixed_exit", "drug_exit")
  )
  checkmate::assertNumeric(age, lower = 1)
  checkmate::assertIntegerish(requiredObservation, lower = 0, any.missing = FALSE, len = 2)
  checkmate::assertTRUE(all(entryDomains %in% c(
    "condition", "drug", "procedure", "observation",
    "measurement", "visit", "device"
  )))
  checkmate::assert(
    checkmate::assertTRUE(all(attritionDomains %in% c(
      "condition", "drug", "procedure", "observation",
      "measurement", "visit", "device"
    ))),
    checkmate::assertNull(attritionDomains)
  )
  checkmate::assertTRUE(rlang::is_installed("rstudioapi"))
  checkmate::assertTRUE(rlang::is_installed("styler"))
  limit <- snakecase::to_any_case(checkmate::matchArg(limit, c("first", "all", "last")), case = "title")
  temp <- templatingCapr()
  .res <- paste0(
    temp$entry,
    .entry = buildEntryEvent(
      entryDomains = entryDomains,
      firstTime = firstTime,
      requiredObservation = requiredObservation,
      age = age,
      Male = dplyr::if_else(gender == "male", TRUE, FALSE),
      Female = dplyr::if_else(gender == "female", TRUE, FALSE),
      limit = limit
    ),
    temp$attrition,
    .attrition = buildAttrition(
      attritionDomains = attritionDomains,
      limit = limit
    ),
    temp$exit,
    buildExit(exitStrategy),
    temp$exitClose
  ) |> styler::style_text()
  styler::cache_clear(ask = FALSE)
  if (copyToClipboard) usethis::ui_code_block(.res) # write(.res, file = rstudioapi::getSourceEditorContext()$path, append = TRUE)
  return(.res)
}


buildEntryEvent <- function(
    entryDomains = "condition",
    requiredObservation = c(0, 0),
    firstTime = FALSE,
    limit,
    age = 18,
    Male = FALSE,
    Female = FALSE) {
  checkmate::assert(isFALSE(Male), isFALSE(Female))
  requiredObservation <-
    glue("observationWindow = Capr::continuousObservation(priorDays = {requiredObservation[[1]]}, postDays = {requiredObservation[[2]]})")
  primaryLimit <-
    glue("primaryCriteriaLimit = {glue::single_quote(limit)}")
  qualifLimit <-
    glue("qualifiedLimit = {glue::single_quote(limit)}")


  occurrenceStart <- "Capr::startDate(Capr::bt(as.Date('2000-01-01'), as.Date('2023-12-31')))\n"
  unionEntry <- function(.entry, order) {
    attr_os <- glue("\n\t\t{occurrenceStart}")
    attr_fo <- glue("{ifelse(firstTime, 'Capr::firstOccurrence()', 'NULL')}")
    attr_age <- "\n\t\tCapr::age(Capr::gte(18))"
    order_1_m <- glue("{ifelse(Male, 'Capr::male()', 'NULL')}")
    order_1_f <- glue("{ifelse(Female, 'Capr::female()', 'NULL')}")
    ll <- c(attr_os, attr_fo, attr_age, order_1_m, order_1_f) |>
      lapply(function(.l) {
        if (.l != "NULL") {
          return(.l)
        }
      }) |>
      purrr::compact()

    attrs <- glue::glue_collapse(ll, sep = ", ")
    return(glue("\nCapr::{.entry}(\t\tCapr::cs(1, name = 'plug'), \n {attrs})"))
  }
  entries <- purrr::map_chr(entryDomains, ~ glue::glue("{capr_ref(.x)}"))
  entr <- purrr::map2_chr(entries, seq_along(entries), unionEntry) |>
    glue::glue_collapse(sep = "\n, ") |>
    paste(
      requiredObservation,
      primaryLimit,
      qualifLimit,
      sep = "\n, "
    )
}

buildAttrition <- function(
    attritionDomains = "condition",
    limit) {
  unionAttriton <- function(.attrition, order) {
    attr_nm <- glue::glue("\t\t{.attrition}_{order}")
    attr_cr <- glue::glue("Capr::{.attrition}(Capr::cs(1, name = 'plug'))")
    attr_bf <- "\t\tCapr::duringInterval(Capr::eventStarts(-Inf, 0))"
    attrs <- c(
      glue("{attr_nm} = Capr::withAll(\n\t\t Capr::atLeast(\n 1"),
      attr_cr,
      attr_bf
    ) |> glue::glue_collapse(sep = ", \n")
    return(glue("{attrs}))"))
  }
  attritions <- purrr::map_chr(attritionDomains, ~ glue::glue("{capr_ref(.x)}"))
  attri <- purrr::map2_chr(
    attritions,
    seq_along(attritions),
    unionAttriton
  ) |>
    glue::glue_collapse(sep = ",") |>
    paste(
      glue("{dplyr::if_else(rlang::is_empty(attritions), '', ',')}expressionLimit = {glue::single_quote(limit)}")
    )
  return(attri)
}

buildExit <- function(end = "observation_period_end_date") {
  .exit <- gsub(
    "_period_end_date", "_exit",
    checkmate::matchArg(
      end, c(
        "observation_period_end_date",
        "fixed_exit",
        "drug_exit"
      )
    )
  )
  .exit <- SqlRender::snakeCaseToCamelCase(
    gsub("event_end_date()", "fixed_exit", .exit)
  )
  endStrategy <- paste0("Capr::", .exit)
  if (.exit == "fixedExit") {
    endStrategy <- paste0(endStrategy, "(
    index = c('startDate',
    offsetDays = 1)")
  } else if (.exit == "drugExit") {
    endStrategy <- paste0(endStrategy, "(
            conceptSet = Capr::cs(1, name = 'plug'),
            persistenceWindow = 30L,
            surveillanceWindow = 0L,
            daysSupplyOverride = NULL
    )")
  } else {
    endStrategy <- paste0(endStrategy, "()")
  }
  return(endStrategy)
}


templatingCapr <- function() {
  entry <- "template <- Capr::cohort(entry = Capr::entry("
  attrition <- "),attrition = Capr::attrition(\n\t\t"
  exit <- "),exit = Capr::exit(\n\t\t endStrategy = "
  exitClose <- "))"
  return(
    list(
      entry = entry,
      attrition = attrition,
      exit = exit,
      exitClose = exitClose
    )
  )
}
