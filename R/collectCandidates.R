
#' Function Collects Reg-ex Matches of Chosen Domain
#'
#' @param con A connection to an OMOP CDM database
#' @param vocabularyDatabaseSchema   Schema name where your OMOP vocabulary format resides. Note that
#'                                   for SQL Server, this should include both the database and schema
#'                                   name, for example 'vocabulary.dbo'.
#' @param keywords case independent  string pattern to search
#' @param exclude case independent  string pattern to exclude
#' @param domains case independent  domain_id to search
#'
#' @return Capr concept set
#' @export
#'
#' @examples
#' \dontrun{
#' con <- DatabaseConnector::connect(connectionDetails)
#' caprCand <- tidyOhdsiRecipies::collectCandidatesToCapr(
#' con, 'cdm5', c('lobar'))
#' }
collectCandidatesToCapr <- function(
    con,
    vocabularyDatabaseSchema,
    keywords,
    exclude = NULL,
    domains = "Condition"
    ) {
  sql_in_clause <- paste0("'", paste(stringr::str_to_lower(domains), collapse = "','"), "'")
  likePattern <- paste0('%', paste(
    stringr::str_split_1(stringr::str_to_lower(keywords), ' '),
    collapse = "%"), '%')
  if (!is.null(exclude)) {
    exclude <- paste0('%', paste(
      stringr::str_split_1(stringr::str_to_lower(exclude), ' '),
      collapse = "%"), '%')
    }

  df <- DatabaseConnector::renderTranslateQuerySql(
    connection = con,
    sql = "
    SELECT distinct *
    FROM @vocab_db_schema.concept
    WHERE lower(domain_id) IN (@domains)  AND
    LIKE(lower(concept_name), '@like_pattern')
    AND standard_concept IN ('S')
    @should_ex AND LIKE(lower(concept_name), '@unlike_pattern')
    ",
    vocab_db_schema = vocabularyDatabaseSchema,
    domains = sql_in_clause,
    like_pattern = likePattern,
    unlike_pattern = exclude,
    should_ex = dplyr::if_else(is.null(exclude), '--', '')
  ) |> dplyr::rename_all(tolower) |>
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
  x <- Capr::cs(df$concept_id, name = 'conceptIdCandidates')
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
