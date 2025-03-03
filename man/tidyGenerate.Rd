% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tidyDarwin.R
\name{tidyGenerate}
\alias{tidyGenerate}
\title{Generate Cohorts from CDM}
\usage{
tidyGenerate(cdm, cohortSet, name, computeAttrition = TRUE, overwrite = TRUE)
}
\arguments{
\item{cdm}{cdm_reference via CDMConnector.}

\item{cohortSet}{A data frame containing the cohort definitions, including JSON expressions. \code{CDMConnector::readCohortSet} output}

\item{name}{The base name for the cohort tables.}

\item{computeAttrition}{boolean. Should attrition be calculated?}

\item{overwrite}{boolean.  Should table be overwritten?}
}
\value{
A list of generated cohorts.
}
\description{
This function generates cohorts from a given CDM and cohort set, and writes the results to the specified schema.
}
\examples{
\dontrun{
cdm <- CDMConnector::cdmFromCon(con, cdmSchema = "cdm_schema")
cohortSet <- data.frame(
  cohortId = 1:2,
  cohortName = c("Cohort 1", "Cohort 2"),
  json = c("{...}", "{...}")
)
generatedCohorts <- tidyGenerate(cdm, cohortSet, "cohort_table_name")
}

}
