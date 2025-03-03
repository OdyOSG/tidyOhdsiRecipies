% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tidyOhdsiMock.R
\name{returnSqLiteDatabaseConnectorCon}
\alias{returnSqLiteDatabaseConnectorCon}
\title{Create Test SQLite Database Connection}
\usage{
returnSqLiteDatabaseConnectorCon()
}
\value{
A DatabaseConnector connection object.
}
\description{
This function creates and returns a connection to a SQLite database using the DatabaseConnector package.
}
\examples{
con <- returnSqLiteDatabaseConnectorCon()
DBI::dbListTables(con)
DatabaseConnector::disconnect(con)
}
