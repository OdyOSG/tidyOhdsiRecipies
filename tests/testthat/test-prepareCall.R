testthat::test_that("Test prepare call function", {
  .res <- eval(tidyOhdsiRecipies::prepareCall(
    'stringr::str_detect', list(
      string = 'conceptSet', pattern = 'ept')))
  testthat::expect_true(.res)
})
