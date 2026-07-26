testthat::test_that("format detection handles supported compression and auxiliary catalogs", {
  testthat::expect_equal(detect_source_format("study.csv.gz"), "csv")
  testthat::expect_equal(detect_source_format("study.dta.xz"), "dta")
  testthat::expect_equal(detect_source_format("study.sav.bz2"), "sav")
  testthat::expect_true(is.na(detect_source_format("study.xlsx.gz")))
  testthat::expect_equal(detect_auxiliary_source_format("formats.sas7bcat"), "sas7bcat")
  testthat::expect_equal(source_stem("study.dta.gz"), "study")
})

testthat::test_that("dataset identifiers are concise for workbook sheets", {
  records <- list(list(
    source_file = "/tmp/workbook.xlsx",
    source_sheet = "employees",
    source_object = NA_character_,
    dataset_name = "workbook__employees"
  ))
  identified <- assign_dataset_ids(records)
  testthat::expect_equal(identified[[1L]]$dataset_id, "workbook_employees")
})

testthat::test_that("the supported-format matrix retains every ingestion family", {
  formats <- supported_formats_table()
  testthat::expect_true(all(c(
    "csv", "xlsx", "dta", "sas7bdat", "xpt", "sav", "rds", "rdata",
    "json", "jsonl", "parquet", "feather", "arrow", "ipc", "pickle"
  ) %in% formats$format))
  testthat::expect_equal(formats$availability[formats$format == "pickle"], "optional; disabled unless explicitly enabled")
})

testthat::test_that("artifact groups preserve the dictionary-metadata distinction", {
  paths <- c(
    "csv/data_dictionary.csv",
    "csv/variable_dictionary.csv",
    "csv/categorical_levels.csv",
    "metadata.json",
    "metadata_report.html",
    "open_science_metadata_report.html",
    "resolved_config.yml"
  )
  testthat::expect_equal(
    artifact_group_for_path(paths),
    c("dictionary", "dictionary", "dictionary + metadata", "metadata", "combined report", "public report", "control")
  )
})

testthat::test_that("missing optional package versions use a display dash", {
  testthat::expect_equal(package_version_safe("package_that_does_not_exist_12345"), "-")
})

testthat::test_that("project metadata profiles round-trip through the local helper", {
  profile <- tempfile(fileext = ".yml")
  helper <- file.path(repo_root, "scripts", "manage_project_metadata.R")
  rscript <- file.path(R.home("bin"), "Rscript")
  values <- c(
    "Profile Test",
    "Public project description",
    "Research Team (2026). Preferred citation.",
    "CC BY 4.0",
    "Controlled",
    "Apply through the study access process.",
    "open science, FAIR"
  )

  write_output <- system2(
    rscript,
    args = c("--vanilla", shQuote(helper), "write", shQuote(profile), shQuote(values)),
    stdout = TRUE,
    stderr = TRUE
  )
  testthat::expect_null(attr(write_output, "status"))
  testthat::expect_true(file.exists(profile))

  metadata <- yaml::read_yaml(profile)$metadata
  testthat::expect_equal(metadata$project_name, values[[1L]])
  testthat::expect_equal(metadata$project_description, values[[2L]])
  testthat::expect_equal(metadata$author, values[[3L]])
  testthat::expect_equal(metadata$license, values[[4L]])
  testthat::expect_equal(metadata$access_classification, values[[5L]])
  testthat::expect_equal(metadata$access_permissions, values[[6L]])
  testthat::expect_equal(unlist(metadata$tags, use.names = FALSE), c("open science", "FAIR"))

  read_output <- system2(
    rscript,
    args = c("--vanilla", shQuote(helper), "read", shQuote(profile)),
    stdout = TRUE,
    stderr = TRUE
  )
  testthat::expect_null(attr(read_output, "status"))
  testthat::expect_equal(
    unname(read_output),
    c(values[[2L]], values[[3L]], values[[4L]], values[[5L]], values[[6L]], "open science, FAIR")
  )

  json_output <- system2(
    rscript,
    args = c("--vanilla", shQuote(helper), "read-json", shQuote(profile)),
    stdout = TRUE,
    stderr = TRUE
  )
  testthat::expect_null(attr(json_output, "status"))
  json_metadata <- jsonlite::fromJSON(paste(json_output, collapse = "\n"))
  testthat::expect_equal(json_metadata$project_description, values[[2L]])
  testthat::expect_equal(json_metadata$author, values[[3L]])
  testthat::expect_equal(json_metadata$tags, "open science, FAIR")

  updated_values <- values
  updated_values[[2L]] <- "Updated public project description"
  update_output <- system2(
    rscript,
    args = c("--vanilla", shQuote(helper), "write", shQuote(profile), shQuote(updated_values)),
    stdout = TRUE,
    stderr = TRUE
  )
  testthat::expect_null(attr(update_output, "status"))
  previous_profile <- sub("\\.yml$", ".previous.yml", profile)
  testthat::expect_true(file.exists(previous_profile))
  testthat::expect_equal(
    yaml::read_yaml(previous_profile)$metadata$project_description,
    values[[2L]]
  )
  testthat::expect_equal(
    yaml::read_yaml(profile)$metadata$project_description,
    updated_values[[2L]]
  )
})
