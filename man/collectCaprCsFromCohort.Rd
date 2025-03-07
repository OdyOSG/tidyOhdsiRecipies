% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/useCohortConceptSets.R
\name{collectCaprCsFromCohort}
\alias{collectCaprCsFromCohort}
\title{Collect Capr Concept Sets from Cohort}
\usage{
collectCaprCsFromCohort(cohortDonor)
}
\arguments{
\item{cohortDonor}{A list containing a cohort donor \code{jsonlite::read_json} or \code{CirceR} output definition}
}
\value{
A named list of concept sets.
}
\description{
This function collects concept sets from a given cohort donor and returns a named list of collection
Capr ConceptSet s4 class.
}
\examples{
\dontrun{
cohortDonor <- jsonlite::read_json(fs::path(
  fs::path_package("tidyOhdsiRecipies"), "cohorts", "PHN.json"
))
caprConceptSets <- tidyOhdsiRecipies::collectCaprCsFromCohort(cohortDonor)[c(1:2)]
cohs <- purrr::map(
  caprConceptSets,
  ~ tidyOhdsiRecipies::createCaprConceptSetCohort(.x)
)
}
}
