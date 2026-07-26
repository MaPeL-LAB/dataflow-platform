#!/usr/bin/env Rscript

file_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
script_path <- sub("^--file=", "", file_arg[[1L]])
repo_root <- normalizePath(file.path(dirname(script_path), ".."), winslash = "/", mustWork = TRUE)
source(file.path(repo_root, "R", "common.R"))

args <- parse_cli_args()
with_optional <- as_flag(args$with_optional, FALSE)
with_tests <- as_flag(args$ci, FALSE) || as_flag(args$with_tests, FALSE)

required <- c("yaml", "readr", "readxl", "haven", "jsonlite", "openxlsx")
optional <- c("arrow", "reticulate")
packages <- required
if (with_optional) packages <- c(packages, optional)
if (with_tests) packages <- c(packages, "testthat")
packages <- unique(packages)

missing <- packages[!vapply(packages, requireNamespace, quietly = TRUE, FUN.VALUE = logical(1L))]
if (length(missing) == 0L) {
  log_info("All requested R packages are already installed.")
  quit(status = 0L)
}

repos <- getOption("repos")
if (is.null(repos) || is.null(repos[["CRAN"]]) || identical(repos[["CRAN"]], "@CRAN@")) {
  repos <- c(CRAN = "https://cloud.r-project.org")
}

log_info("Installing: ", paste(missing, collapse = ", "))
utils::install.packages(missing, repos = repos, dependencies = TRUE)

still_missing <- missing[!vapply(missing, requireNamespace, quietly = TRUE, FUN.VALUE = logical(1L))]
if (length(still_missing) > 0L) {
  stop(
    "Installation did not complete for: ", paste(still_missing, collapse = ", "),
    ". Optional packages may require system libraries; see README.md.",
    call. = FALSE
  )
}
log_info("Bootstrap complete.")
