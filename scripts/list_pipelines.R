#!/usr/bin/env Rscript

file_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
script_path <- sub("^--file=", "", file_arg[[1L]])
repo_root <- normalizePath(file.path(dirname(script_path), ".."), winslash = "/", mustWork = TRUE)
source(file.path(repo_root, "R", "common.R"))
source(file.path(repo_root, "R", "pipeline_registry.R"))

pipelines <- list_registered_pipelines(repo_root)
print(pipelines, row.names = FALSE, right = FALSE)
