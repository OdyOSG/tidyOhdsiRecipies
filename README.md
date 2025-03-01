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

    remotes::install_github('OdyOSG/tidyOhdsiRecipies')

## Example

This is a basic example which shows you how to solve a common problem:

    library(tidyOhdsiRecipies)
    # create cohortsToCreate for `CohortGenerator` package 
    cohortsToCreate <- tidyOhdsiRecipies::createCohortsToCreate(
      path = 'inst/cohorts',
      computeAttrition = TRUE
    )
