#!/usr/bin/env Rscript

source("scripts/generate_examples.R")
out <- tempfile("data_dictionary_test_")
system2("Rscript", c("scripts/run_data_dictionary.R", "--input", "examples/data/people.csv", "--output", out, "--max-categorical-levels", "10"), stdout = TRUE, stderr = TRUE)
required <- c("variable_dictionary.csv", "dataset_metadata.csv", "categorical_levels.csv", "quality_issues.csv", "metadata.json", "metadata.xlsx", "metadata_report.html", "resolved_config.yml")
stopifnot(all(file.exists(file.path(out, required))))
d <- readr::read_csv(file.path(out, "variable_dictionary.csv"), show_col_types = FALSE)
name_row <- d[d$variable_name == "person_name", ]
status_row <- d[d$variable_name == "status", ]
stopifnot(nrow(name_row) == 1L, !isTRUE(name_row$categorical_eligible), name_row$n_unique_non_missing > 10)
stopifnot(nrow(status_row) == 1L, isTRUE(status_row$categorical_eligible))
stopifnot(name_row$examples == "<masked>")
message("All smoke tests passed.")
