% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/conceptSetTransition.R
\name{listConceptIdsFromCs}
\alias{listConceptIdsFromCs}
\title{Prepare List of Concept Ids using a vocab From Concept Set}
\usage{
listConceptIdsFromCs(x, con, vocabularyDatabaseSchema)
}
\arguments{
\item{x}{A concept set}

\item{con}{A connection to an OMOP CDM database}

\item{vocabularyDatabaseSchema}{Schema name where your OMOP vocabulary format resides. Note that
for SQL Server, this should include both the database and schema
name, for example 'vocabulary.dbo'.}
}
\value{
A named list contains numeric vector of concept ids
}
\description{
Concept sets created in R using the \code{cs} function of collected from donor cohort
or created by \code{Capr::readConceptSet} function
}
\examples{
\dontrun{
# create a concept set
diclofenac <- cs(descendants(1124300), name = "diclofenac")

# fill in the details from an OMOP CDM
library(DatabaseConnector)
con <- connect(dbms = "postgresql", user = "postgres", password = "", server = "localhost/cdm")
diclofenacConceptIds <- listConceptIdsFromCs(diclofenac, con, vocabularyDatabaseSchema = "cdm5")
}
}
