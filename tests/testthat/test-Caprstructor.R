testthat::test_that("Caprstructor", {
  res <- tidyOhdsiRecipies::writeCaprstructor()
  testthat::expect_s3_class(res, "vertical")
})

