
# tidyOhdsiRecipies <img src="man/figures/logo.jpg" align="right" height="92" alt="" />

<!-- README.md is generated from README.Rmd. Please edit that file -->

# tidyOhdsiRecipies

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/tidyOhdsiRecipies)](https://CRAN.R-project.org/package=tidyOhdsiRecipies)
[![Codecov test
coverage](https://codecov.io/gh/OdyOSG/tidyOhdsiRecipies/graph/badge.svg)](https://app.codecov.io/gh/OdyOSG/tidyOhdsiRecipies)
<!-- badges: end -->

The goal of tidyOhdsiRecipies is to simplify and automate routine tasks
when working with the Observational Medical Outcomes Partnership (OMOP)
Common Data Model (CDM). This package provides a set of tools and
functions to help researchers and analysts effectively manage, analyze
and modify data that complies with the OMOP CDM standard.

## Installation

You can install the development version of tidyOhdsiRecipies like so:

``` r
# install.packages('remotes')
remotes::install_github('OdyOSG/tidyOhdsiRecipies')
```

## Example

Reading json cohorts representation to `CohortGenerator` definition set
and / or convert it to Darwin `CDMConnector` cohort set in case your are
using `PhenotypeLibrary` package

``` r
library(tidyOhdsiRecipies)
# create cohortsToCreate for `CohortGenerator` package
# Case 1
cohortsToCreate <- tidyOhdsiRecipies::createCohortDefinitionSet(
  path = "inst/cohorts",
  computeAttrition = TRUE
)

### if you want to use Darwin
cohortSet <- cohortsToCreate2CDMConn(cohortsToCreate)
# Case 2
# You need to build 100 covariate cohorts from concept sets that have different domains and contain both standard and source codes.
caprConceptSets <- collectCaprCsFromCohort(returnTestDonorCohort())
cohorts <- purrr::map(
  caprConceptSets,
  ~ createCaprConceptSetCohort(
    conceptSet = .x,
    limit = "all",
    requiredObservation = c(1, 1),
    end = "fixed_exit",
    endArgs = list(
      index = c("startDate"),
      offsetDays = 1
    ),
    addSourceCriteria = TRUE
  )
)
## then you can deliver it to cohortsToCreate
cohortsToCreate <- createCohortDefinitionSet(
  cohorts = cohorts
)
```
