#!/usr/bin/env Rscript

file_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
script_path <- sub("^--file=", "", file_arg[[1L]])
repo_root <- normalizePath(file.path(dirname(script_path), ".."), winslash = "/", mustWork = TRUE)
source(file.path(repo_root, "R", "common.R"))

args <- parse_cli_args()
output_dir <- path.expand(as.character(args$output %||% file.path(repo_root, "outputs", "dataminima-fixture"))[[1L]])
for (package in c("yaml", "readr", "readxl", "haven", "jsonlite", "jsonvalidate", "openxlsx", "openssl", "zip")) {
  require_package(package, "the generated DataMinima integration fixture")
}

pipeline_dir <- file.path(repo_root, "pipelines", "data_dictionary")
for (module in c("config.R", "utils.R", "ingest.R", "classify.R", "profile.R", "dictionary.R", "report.R", "handoff.R", "export.R", "pipeline.R")) {
  source(file.path(pipeline_dir, "R", module))
}
source(file.path(pipeline_dir, "tests", "fixtures", "dataminima_fixture.R"))

fixture_dir <- tempfile("dataflow-dataminima-generated-fixture-")
on.exit(unlink(fixture_dir, recursive = TRUE, force = TRUE), add = TRUE)
log_info("Building generated-only DataMinima fixture; no private or real dataset is used.")
run_dataminima_fixture(
  root = fixture_dir,
  output = output_dir,
  default_config_path = file.path(pipeline_dir, "config", "default.yml")
)

bundle <- normalizePath(file.path(output_dir, "dataminima_handoff.zip"), winslash = "/", mustWork = TRUE)
review <- normalizePath(file.path(output_dir, "dataminima_disclosure_review.json"), winslash = "/", mustWork = TRUE)
cat("\nGenerated fixture bundle:\n  ", bundle, "\n", sep = "")
cat("Disclosure review evidence:\n  ", review, "\n", sep = "")
workspace_root <- dirname(dirname(repo_root))
dataminima_root <- file.path(workspace_root, "research-ai-products", "synthetic-data-platform")
if (file.exists(file.path(dataminima_root, ".venv", "bin", "python"))) {
  qualification <- file.path(output_dir, "dataflow_qualification.json")
  cat(
    "Next command:\n  cd ", shQuote(dataminima_root),
    " && PYTHONPATH=src .venv/bin/python -m metadata_synthetic_data_platform.cli qualify-dataflow-bundle --bundle ",
    shQuote(bundle), " --output ", shQuote(qualification), "\n",
    sep = ""
  )
}
