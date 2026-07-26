repo_root <- normalizePath(file.path(testthat::test_path(), "..", ".."), winslash = "/", mustWork = TRUE)
source(file.path(repo_root, "R", "common.R"))
source(file.path(repo_root, "R", "pipeline_registry.R"))
