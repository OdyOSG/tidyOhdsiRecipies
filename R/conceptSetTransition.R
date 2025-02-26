#' Convert Concept Set Expression to Capr Concept Set
#'
#' This function converts a named list containing tibbles of concept set expression details into a Capr concept set.
#'
#' @param x A named list containing tibbles of concept set expression details.
#'
#' @return A Capr concept set.
#' @export
#'
#' @examples
#' # Example usage:
#' caprCs <- conceptSetExpression2CaprCs(
#'   list(test_set = dplyr::tibble(
#'     concept_id = c(1, 2),
#'     excluded = c(FALSE, TRUE),
#'     descendants = c(TRUE, FALSE),
#'     mapped = c(FALSE, TRUE)
#'   ))
#' )
conceptSetExpression2CaprCs <- function(x) {
  .nm <- names(x)
  x <- purrr::pluck(x, .nm)
  items <- purrr::map(
    seq_along(x$concept_id), function(id) {
      list(
        concept = list(
          CONCEPT_ID = x$concept_id[id],
          CONCEPT_NAME = "", STANDARD_CONCEPT = "",
          STANDARD_CONCEPT_CAPTION = "", INVALID_REASON = "",
          INVALID_REASON_CAPTION = "", CONCEPT_CODE = "",
          DOMAIN_ID = "", VOCABULARY_ID = "", CONCEPT_CLASS_ID = ""
        ),
        isExcluded = x$excluded[id], includeDescendants = x$descendants[id],
        includeMapped = x$mapped[id]
      )
    }
  ) |>
    .removeItemDuplicates() |>
    purrr::map(~ newConcept(
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
  rlang::inject(Capr::cs(!!!items, name = .nm))
}


#' Prepare List of Concept Ids using a vocab From Concept Set
#'
#' Concept sets created in R using the `cs` function of collected from donor cohort
#' or created by `Capr::readConceptSet` function
#'
#' @param x A concept set
#' @param con A connection to an OMOP CDM database
#' @param vocabularyDatabaseSchema   Schema name where your OMOP vocabulary format resides. Note that
#'                                   for SQL Server, this should include both the database and schema
#'                                   name, for example 'vocabulary.dbo'.
#' @return A named list contains numeric vector of concept ids
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # create a concept set
#' diclofenac <- cs(descendants(1124300), name = "diclofenac")
#'
#' # fill in the details from an OMOP CDM
#' library(DatabaseConnector)
#' con <- connect(dbms = "postgresql", user = "postgres", password = "", server = "localhost/cdm")
#' diclofenacConceptIds <- listConceptIdsFromCs(diclofenac, con, vocabularyDatabaseSchema = "cdm5")
#' }
listConceptIdsFromCs <- function(x, con, vocabularyDatabaseSchema) {

  checkmate::assert(DBI::dbIsValid(con))
  checkmate::assert_class(x, 'ConceptSet')
  checkmate::assert_character(vocabularyDatabaseSchema)

  .tibb <- .capr2Tibble(x)
  nm <- x@Name
  ancestorConceptIdsIncl <- .tibb |>
    dplyr::filter(
      .data$includeDescendants == TRUE &
        .data$isExcluded == FALSE
    ) |> dplyr::pull(.data$concept_id)

  ancestorConceptIdsExcl <- .tibb |>
    dplyr::filter(
      .data$includeDescendants == TRUE &
        .data$isExcluded == TRUE
    ) |>
    dplyr::pull(.data$concept_id)
  includeNonDesc <- .tibb |>
    dplyr::filter(
      .data$isExcluded == FALSE &
        .data$includeDescendants == FALSE
    ) |>
    dplyr::pull(.data$concept_id)
  excludeNonDesc <- .tibb |>
    dplyr::filter(
      .data$isExcluded == TRUE &
        .data$includeDescendants == FALSE
    ) |>
    dplyr::pull(.data$concept_id)
  if (rlang::is_empty(ancestorConceptIdsIncl)) ancestorConceptIdsIncl <- -1L
  if (rlang::is_empty(ancestorConceptIdsExcl)) ancestorConceptIdsExcl <- -1L
  if (rlang::is_empty(includeNonDesc)) includeNonDesc <- -1L
  if (rlang::is_empty(excludeNonDesc)) excludeNonDesc <- -1L

  conceptsDesc <- DatabaseConnector::renderTranslateQuerySql(
    connection = con,
    sql = "
    SELECT include_table.descendant_concept_id ids
    FROM (
    select distinct concept_id descendant_concept_id
      FROM @vocabulary_database_schema.CONCEPT
      where concept_id in (
      @include_not_desc, @ancestor_concept_ids_incl)
    union
    select distinct descendant_concept_id
    FROM @vocabulary_database_schema.CONCEPT_ANCESTOR
    where ancestor_concept_id in (@ancestor_concept_ids_incl)
    ) include_table
    LEFT JOIN (select distinct descendant_concept_id
      FROM @vocabulary_database_schema.CONCEPT_ANCESTOR
      where ancestor_concept_id in (@ancestor_concept_ids_excl)
      UNION
      select distinct concept_id descendant_concept_id
      FROM @vocabulary_database_schema.CONCEPT
      where concept_id in (@exclude_not_desc)
      )
      exclude_table ON
      include_table.descendant_concept_id = exclude_table.descendant_concept_id
    AND exclude_table.descendant_concept_id IS NULL;",
    vocabulary_database_schema = vocabularyDatabaseSchema,
    ancestor_concept_ids_incl = ancestorConceptIdsIncl,
    ancestor_concept_ids_excl = ancestorConceptIdsExcl,
    include_not_desc = includeNonDesc,
    exclude_not_desc = excludeNonDesc
  )$IDS

  if (rlang::is_empty(conceptsDesc)) {
    cli::cli_abort('There is no concept ids')
  }
  res <- list()
  res[[nm]] <- conceptsDesc
  return(res)
  }

.capr2Tibble <- function(x) {
  df <- dplyr::tibble(
    concept_set_name = x@Name,
    concept_id = purrr::map_int(
    x@Expression,
    ~ .@Concept@concept_id
  ), concept_name = purrr::map_chr(
    x@Expression,
    ~ .@Concept@concept_name
  ), domain_id = purrr::map_chr(
    x@Expression,
    ~ .@Concept@domain_id
  ), vocabulary_id = purrr::map_chr(
    x@Expression,
    ~ .@Concept@vocabulary_id
  ), concept_class_id = purrr::map_chr(
    x@Expression,
    ~ .@Concept@concept_class_id
  ), standard_concept = purrr::map_chr(
    x@Expression,
    ~ .@Concept@standard_concept
  ), standard_concept_caption = purrr::map_chr(
    x@Expression,
    ~ .@Concept@standard_concept_caption
  ), concept_code = purrr::map_chr(
    x@Expression,
    ~ .@Concept@concept_code
  ), invalid_reason = purrr::map_chr(
    x@Expression,
    ~ .@Concept@invalid_reason
  ), invalid_reason_caption = purrr::map_chr(
    x@Expression,
    ~ .@Concept@invalid_reason_caption
  ), includeDescendants = purrr::map_lgl(
    x@Expression,
    "includeDescendants"
  ), isExcluded = purrr::map_lgl(
    x@Expression,
    "isExcluded"
  ), includeMapped = purrr::map_lgl(
    x@Expression,
    "includeMapped"
  ))
}
