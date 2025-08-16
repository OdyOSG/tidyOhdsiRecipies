# Script to rebuild README.md from README.Rmd
rmarkdown::render("README.Rmd", output_format = "github_document")