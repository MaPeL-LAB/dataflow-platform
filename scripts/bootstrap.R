#!/usr/bin/env Rscript

required <- c("yaml", "readr", "readxl", "haven", "jsonlite", "openxlsx", "testthat")
optional <- c("arrow")
args <- commandArgs(trailingOnly = TRUE)
if ("--with-optional" %in% args) required <- c(required, optional)
missing <- required[!vapply(required, requireNamespace, quietly = TRUE, FUN.VALUE = logical(1))]
if (!length(missing)) {
  message("All requested packages are installed.")
  quit(status = 0)
}
install.packages(missing, repos = "https://cloud.r-project.org")
