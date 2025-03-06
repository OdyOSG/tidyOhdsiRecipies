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

#' Collects Concept Sets or Concept Ids from ingredients
#'
#' @param ingredients an ingredient character vector
#' @param con DBI or DatabaseConnector connection to database
#' @param vocabularyDatabaseSchema CDM schema with vocabulary tables
#' @param return define what to return list of Capr or list of concept ids
#'
#' @return either named list of Capr concept sets or named list of numeric vectors with descendant concept ids
#' @export
#'
#' @examples
#' \dontrun{
#' cs <- collectIngredientConceptIds(
#' 'vedolizumab', con, 'cdm5', 'Capr')
#' }
collectIngredientConceptIds <- function(
    ingredients, con, vocabularyDatabaseSchema,
    return = c('Capr', 'descendantIds')
    ) {
  checkmate::assert(DBI::dbIsValid(con))

  checkmate::assertCharacter(ingredients, any.missing = FALSE,
                           min.len = 1)

  checkmate::assertCharacter(vocabularyDatabaseSchema)

  checkmate::assertChoice(return, c('Capr', 'descendantIds'))
  if (return == 'descendantIds') {
    sqls <- purrr::map(ingredients, ~ SqlRender::render(
      "select distinct descendant_concept_id
     from @vocab_schema.concept_ancestor
     where ancestor_concept_id IN (
    select concept_id from @vocab_schema.concept
    where
    standard_concept IN ('S') and
    concept_class_id = 'Ingredient' and
    domain_id = 'Drug' and lower(concept_name) IN (@x)
     )",
      vocab_schema = vocabularyDatabaseSchema,
      x = glue::single_quote(tolower(.x))
    ) |> SqlRender::translate(targetDialect = dbms(con))
    )
    desc <- purrr::map(sqls, ~suppressWarnings(DBI::dbGetQuery(con, .x, immediate = TRUE)) |>
      dplyr::pull(.data$descendant_concept_id)) |>
      rlang::set_names(paste0('i_', ingredients))
    return(desc)
  } else if (return == 'Capr') {
    sqls <- purrr::map(ingredients, ~ SqlRender::render(
      "
      select concept_id from @vocab_schema.concept
      where   standard_concept IN ('S') and
      concept_class_id = 'Ingredient' and
      domain_id = 'Drug' and lower(concept_name) IN (@x)
     ",
      vocab_schema = vocabularyDatabaseSchema,
      x = glue::single_quote(tolower(.x))
    ) |> SqlRender::translate(targetDialect = dbms(con))
    )
    .css <- purrr::map(sqls, function(.x) {
      cId <- suppressWarnings(DBI::dbGetQuery(con, .x, immediate = TRUE)) |>
        dplyr::pull(.data$concept_id)
      .cs <- Capr::cs(Capr::descendants(cId), name = .x) |>
        Capr::getConceptSetDetails(con, vocabularyDatabaseSchema)
    }) |>
      rlang::set_names(paste0('i_', ingredients))
    return(.css)
  }
}

collectIngredients <- function(con, vocabularyDatabaseSchema) {
  sql <- SqlRender::render(
    "select distinct concept_name  from
    @voc_schema.concept
    where standard_concept IN ('S') and
    concept_class_id = 'Ingredient' and
    domain_id = 'Drug'",
    voc_schema = vocabularyDatabaseSchema
  ) |> SqlRender::translate(dbms(con))
  res <- suppressWarnings(DBI::dbGetQuery(con, sql, immediate = TRUE)) |>
    dplyr::pull(.data$concept_name)
}
