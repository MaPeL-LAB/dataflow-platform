#!/usr/bin/env Rscript

file_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
script_path <- sub("^--file=", "", file_arg[[1L]])
repo_root <- normalizePath(file.path(dirname(script_path), ".."), winslash = "/", mustWork = TRUE)
source(file.path(repo_root, "R", "common.R"))
require_package("testthat", "the test suite")

pipeline_dirs <- list.dirs(file.path(repo_root, "pipelines"), full.names = TRUE, recursive = FALSE)
test_dirs <- c(
  file.path(repo_root, "tests", "testthat"),
  file.path(pipeline_dirs, "tests", "testthat")
)

for (path in test_dirs[dir.exists(test_dirs)]) {
  log_info("Testing ", path)
  testthat::test_dir(path, reporter = "summary", stop_on_failure = TRUE)
}
