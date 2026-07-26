#!/usr/bin/env Rscript

file_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
if (length(file_arg) == 0L) stop("This script must be run with Rscript.", call. = FALSE)
script_path <- sub("^--file=", "", file_arg[[1L]])
repo_root <- normalizePath(file.path(dirname(script_path), ".."), winslash = "/", mustWork = TRUE)

source(file.path(repo_root, "R", "common.R"))
pipeline_dir <- file.path(repo_root, "pipelines", "data_dictionary")
source(file.path(pipeline_dir, "R", "config.R"))
source(file.path(pipeline_dir, "R", "utils.R"))
source(file.path(pipeline_dir, "R", "ingest.R"))

args <- parse_cli_args()
required <- c("input", "format", "config", "result")
missing <- required[vapply(required, function(name) is.null(args[[name]]), logical(1L))]
if (length(missing) > 0L) {
  stop("Missing worker argument(s): ", paste(missing, collapse = ", "), call. = FALSE)
}

input_path <- as.character(args$input[[1L]])
format <- as.character(args$format[[1L]])
config <- readRDS(as.character(args$config[[1L]]))
result_path <- as.character(args$result[[1L]])

result <- read_haven_source(input_path, format, config)
saveRDS(result, result_path)
