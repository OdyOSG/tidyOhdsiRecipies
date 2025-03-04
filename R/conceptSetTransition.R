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
#' caprCs <- darwinCSExpression2CaprCs(
#'   list(test_set = dplyr::tibble(
#'     concept_id = c(1, 2),
#'     excluded = c(FALSE, TRUE),
#'     descendants = c(TRUE, FALSE),
#'     mapped = c(FALSE, TRUE)
#'   ))
#' )
darwinCSExpression2CaprCs <- function(x) {
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
  checkmate::assert_class(x, "ConceptSet")
  checkmate::assert_character(vocabularyDatabaseSchema)
  .tibb <- Capr::as.data.frame(x)
  nm <- x@Name
  ancestorConceptIdsIncl <- .tibb |>
    dplyr::filter(
      .data$includeDescendants == TRUE &
        .data$isExcluded == FALSE
    ) |>
    dplyr::pull(.data$conceptId)

  ancestorConceptIdsExcl <- .tibb |>
    dplyr::filter(
      .data$includeDescendants == TRUE &
        .data$isExcluded == TRUE
    ) |>
    dplyr::pull(.data$conceptId)
  includeNonDesc <- .tibb |>
    dplyr::filter(
      .data$isExcluded == FALSE &
        .data$includeDescendants == FALSE
    ) |>
    dplyr::pull(.data$conceptId)
  excludeNonDesc <- .tibb |>
    dplyr::filter(
      .data$isExcluded == TRUE &
        .data$includeDescendants == FALSE
    ) |>
    dplyr::pull(.data$conceptId)
  if (rlang::is_empty(ancestorConceptIdsIncl)) ancestorConceptIdsIncl <- -1L
  if (rlang::is_empty(ancestorConceptIdsExcl)) ancestorConceptIdsExcl <- -1L
  if (rlang::is_empty(includeNonDesc)) includeNonDesc <- -1L
  if (rlang::is_empty(excludeNonDesc)) excludeNonDesc <- -1L
  sql <- SqlRender::render(
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
  ) |> SqlRender::translate(targetDialect = dbms(con))
  conceptsDesc <- DBI::dbGetQuery(con, statement = sql) |>
    dplyr::pull(.data$ids)

  if (rlang::is_empty(conceptsDesc)) {
    cli::cli_abort("There is no concept ids")
  }
  res <- list()
  res[[nm]] <- conceptsDesc
  return(res)
}



#' Union Capr Concept Sets and Concept Ids
#'
#' This function merges concept sets and concepts into a single concept set.
#'
#' @param x A named list of `ConceptSet`
#' @param conceptIds A vector of concept IDs to be included in the merged concept set.
#'
#' @return A `ConceptSet` object containing the merged concepts.
#' @export
#'
#' @examples
#' \dontrun{
#' caprConceptSets <- tidyOhdsiRecipies::collectCaprCsFromCohort(cohortDonor)[1]
#' mergedConceptSet <- mergeCsAndConcepts(caprConceptSets, conceptIds = 1:2)
#' }
mergeCsAndConcepts <- function(x, conceptIds) {
  rlang::check_installed("tidyr")

  checkmate::assertVector(conceptIds,
    any.missing = FALSE,
    unique = TRUE
  )
  checkmate::assertList(
    x,
    types = "ConceptSet",
    names = "named"
  )


  nm <- names(x)
  df <- purrr::map_dfr(
    list(purrr::chuck(x, 1), Capr::cs(conceptIds, name = nm)), Capr::as.data.frame
  ) |>
    dplyr::group_by(.data$conceptId) |>
    dplyr::slice(1) |>
    dplyr::ungroup() |>
    dplyr::rename_all(snakecase::to_snake_case) |>
    dplyr::mutate(name = nm)


  name <- df[["name"]][1] %||% df[["concept_set_name"]][1]
  if (is.na(name) || is.null(name)) {
    name <- "plug"
  }
  conceptDf <- dplyr::tibble(
    id = df[["concept_id"]] %||%
      df[["concept id"]] |> as.integer(),
    isExcluded = df[["is_excluded"]] %||%
      df[["exclude"]] %||% FALSE |> as.logical(),
    includeDescendants = df[["include_descendants"]] %||%
      df[["descendants"]] %||% FALSE |> as.logical(),
    includeMapped = df[["include_mapped"]] %||% df[["mapped"]] %||%
      FALSE |> as.logical(), conceptName = df[["concept_name"]] %||%
      df[["concept name"]] %||% "" |> as.character(),
    standardConcept = df[["standard_concept"]] %||%
      df[["standard concept"]] %||% "" |> as.character(),
    standardConceptCaption = df[["standard_concept"]] %||%
      "" |> as.character(), invalidReason = df[["invalid_reason"]] %||%
      "" |> as.character(), invalidReasonCaption = df[["invalid_reason_caption"]] %||%
      "" |> as.character(), conceptCode = df[["concept_code"]] %||%
      df[["concept code"]] %||% "" |> as.character(),
    domainId = df[["domain_id"]] %||% df[["domain"]] %||%
      "" |> as.character(), vocabularyId = df[["vocabulary_id"]] %||%
      df[["vocabulary"]] %||% "" |> as.character(),
    conceptClassId = df[["concept_class_id"]] %||% "" |>
      as.character()
  ) |> dplyr::mutate_if(
    is.character,
    ~ tidyr::replace_na(.x, "")
  )
  conceptList <- purrr::pmap(conceptDf, newConcept)
  rlang::inject(Capr::cs(!!!conceptList, name = name))
}
