repo_root <- normalizePath(file.path(testthat::test_path(), "..", "..", "..", ".."), winslash = "/", mustWork = TRUE)
source(file.path(repo_root, "R", "common.R"))
pipeline_dir <- file.path(repo_root, "pipelines", "data_dictionary")
for (module in c("config.R", "utils.R", "ingest.R", "classify.R", "profile.R", "dictionary.R", "report.R", "export.R", "pipeline.R")) {
  source(file.path(pipeline_dir, "R", module))
}
default_config_path <- file.path(pipeline_dir, "config", "default.yml")
test_config <- function(overrides = list()) load_data_dictionary_config(default_config_path, overrides = overrides)
